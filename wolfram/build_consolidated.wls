#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/lost_in_translation.wl — one consolidated Wolfram
    Language package containing everything the published notebook needs
    to evaluate its cells:

       - framework.wl       (HTTP transport, embeddings, GPT calls, audit)
       - translator_tool.wl (Audit, RunAudit, TranslationAuditTool)
       - chart-rendering functions for every figure in the notebook
       - all frozen JSON data, embedded as in-package constants

    Output is a single .wl file alongside the .nb. The user uploads two
    files (notebook + .wl) and the post is fully re-runnable.

    The notebook code cells call functions like
        LostInTranslation`DriftTimeline[]
    instead of wolframscript invocations.

    Run once whenever framework.wl, translator_tool.wl, or the frozen
    JSON files change:
        wolframscript -file wolfram/build_consolidated.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir  = FileNameJoin[{repoRoot, "data"}];
wolfDir  = FileNameJoin[{repoRoot, "wolfram"}];
out      = FileNameJoin[{repoRoot, "community", "lost_in_translation.wl"}];

(* --- Slurp the source packages as text and strip their wrappers. --- *)
slurp[name_] := Import[FileNameJoin[{wolfDir, name}], "Text"];

(* Pull body out of `BeginPackage["X`"]` ... `EndPackage[]`.
   We keep the Begin["`Private`"] body intact \[Dash] only drop the
   outer BeginPackage / public declarations / EndPackage. *)
stripPackageWrapper[src_String] := Module[{lines, body},
   lines = StringSplit[src, "\n"];
   (* Drop everything up to and including Begin["`Private`"]; *)
   body = lines /. {___, line_String /; StringContainsQ[line, "Begin[\"`Private`\"]"], rest___} :> {rest};
   (* Drop the trailing End[]; EndPackage[]; and anything after *)
   body = TakeWhile[body,
      ! StringContainsQ[#, "EndPackage[]"] &
        && ! (StringMatchQ[StringTrim[#], "End[];"] && Position[body, #][[-1, 1]] >= Length[body] - 5) &];
   StringRiffle[body, "\n"]
];

(* Robust extractor: substring between the `Begin["\`Private\`"];` line
   and the closing `End[];` line. *)
extractPrivateBody[src_String] := Module[{startMatch, endMatch, start, end},
   startMatch = StringPosition[src, "Begin[\"`Private`\"];"];
   endMatch   = StringPosition[src, "End[];" ~~ Whitespace ~~ "EndPackage"];
   If[startMatch === {} || endMatch === {},
      Print["WARNING: extractPrivateBody fell back to raw src"];
      Return[src]];
   start = Last[First[startMatch]];
   end   = First[Last[endMatch]] - 1;
   StringTake[src, {start + 1, end}]
];

(* Rewrites required when collapsing two packages into one:
   - drop `Needs["TPFramework`"]` (already part of this package);
   - drop the `Set` definitions of $pkgFile / $pkgDir / $repoRoot
     in framework.wl that would reference an InputFileName we no
     longer care about;
   - rewrite TPFramework`Private`X -> X, TPFramework`X -> X,
     TranslatorTool`Private`X -> X, TranslatorTool`X -> X. *)
rewriteForConsolidation[src_String] := Module[{s = src},
   s = StringReplace[s, {
      RegularExpression["Needs\\[\"TPFramework`\"\\];?\\s*"] -> "",
      "TPFramework`Private`"   -> "",
      "TPFramework`"           -> "",
      "TranslatorTool`Private`" -> "",
      "TranslatorTool`"        -> ""}];
   s];

fwBody = rewriteForConsolidation @ extractPrivateBody[slurp["framework.wl"]];
ttBody = rewriteForConsolidation @ extractPrivateBody[slurp["translator_tool.wl"]];

Print["framework.wl  body: ", StringLength[fwBody], " bytes"];
Print["translator_tool body: ", StringLength[ttBody], " bytes"];

(* --- Slurp the frozen JSON data files. We parse them at build time
       so the in-package representation is already a WL expression. This
       avoids round-tripping non-BMP Unicode through string literals. *)
slurpJSON[name_] := Module[{p = FileNameJoin[{dataDir, name}]},
   If[FileExistsQ[p], Import[p, "RawJSON"], Null]];
slurpJSONText[name_] := Module[{p = FileNameJoin[{dataDir, name}]},
   If[FileExistsQ[p], Import[p, "Text"], ""]];

frozenFiles = <|
   "romans8translated"   -> "section_romans-8_translated.json",
   "romans8nuance"       -> "section_romans-8_nuance.json",
   "cor13translated"     -> "section_1-corinthians-13_translated.json",
   "cor13nuance"         -> "section_1-corinthians-13_nuance.json",
   "iliad3way"           -> "iliad_3way.json",
   "tao2way"             -> "tao_2way.json",
   "translationDrift"    -> "translation_drift.json",
   "lengthDivergence"    -> "length_vs_divergence.json",
   "embedComparison"     -> "embedding_comparison.json",
   "translatorExample"   -> "translator_tool_example.json",
   "newsFactChecker"     -> "news_fact_checker.json",
   "historicalBibles"    -> "historical_bibles_romans8.json"
|>;

frozenSizes = Map[StringLength @ slurpJSONText @ # &, frozenFiles];
Print["frozen data totals (raw JSON chars): ",
      Total[Values[frozenSizes]]];

(* Emit each frozen file as a pre-parsed WL expression in InputForm.
   Non-ASCII characters are written as `\[Name]` / `\:XXXX` escapes by
   InputForm, which WL parses back into the original Unicode. We bypass
   the JSON parser at load time entirely. *)
encodedFrozen = StringJoin[
   "(* === Embedded frozen data (pre-parsed at build time, ~",
   ToString @ Round[Total[Values[frozenSizes]]/1024],
   " KB raw JSON) === *)\n\n$frozen = <|\n",
   StringRiffle[
     KeyValueMap[
       Function[{key, filename},
         "  \"" <> key <> "\" -> " <>
         ToString[slurpJSON[filename], InputForm,
                  CharacterEncoding -> "ASCII"]],
       frozenFiles],
     ",\n"],
   "\n|>;\n\n"];
Print["encoded frozen data length: ",
      StringLength[encodedFrozen], " bytes"];

(* --- Chart-rendering function definitions. --- *)
(* Each chart function returns Graphics live, reading from the cached
   frozen data. Copy-pastable from the existing .wls files but adapted
   to use the in-memory cache. *)

chartFunctionsBody = "

(* Lazy JSON parser: parses the raw JSON only on first use, caches the
   result, returns the parsed Association list. *)
parseFrozen[key_String] := $frozen[key];

(* --- DriftTimeline -------------------------------------------------- *)
DriftTimeline[] := Module[
   {d, names, years, driftFromTyndale},
   d = parseFrozen[\"translationDrift\"];
   names = #[\"short\"] & /@ d[\"translations\"];
   years = #[\"year\"] & /@ d[\"translations\"];
   driftFromTyndale = Lookup[d[\"drift_from_baseline\"], names];
   ListLinePlot[
     Transpose[{years, driftFromTyndale}],
     PlotStyle -> Directive[RGBColor[0.55, 0.20, 0.15],
                            Thickness[0.0045]],
     PlotMarkers -> {Automatic, 10},
     Epilog -> MapThread[
       {Black, Disk[{#1, #2}, Offset[5]],
        Text[Style[#3 <> \"\\n\" <> ToString[#1],
              FontFamily -> \"Helvetica\", FontSize -> 10,
              FontColor -> GrayLevel[0.25]],
             {#1, #2}, {0, -1.4}]} &,
       {years, driftFromTyndale, names}],
     Frame -> True, GridLines -> Automatic,
     FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> \"Helvetica\",
                             FontSize -> 13],
     FrameLabel -> {
        Style[\"year\", FontFamily -> \"Helvetica\", FontSize -> 14],
        Style[\"cosine distance from Tyndale 1534\",
              FontFamily -> \"Helvetica\", FontSize -> 14]},
     PlotLabel -> Style[
        \"Cumulative translation drift: Romans 8, Tyndale 1534 baseline\",
        Bold, 15, FontFamily -> \"Helvetica\"],
     ImageSize -> 1100, AspectRatio -> 0.5,
     PlotRange -> {{1525, 2020}, {0, 0.25}}]];

(* --- DriftDendrogram ------------------------------------------------ *)
DriftDendrogram[] := Module[
   {d, names, years, mat, labels},
   d = parseFrozen[\"translationDrift\"];
   names = #[\"short\"] & /@ d[\"translations\"];
   years = #[\"year\"] & /@ d[\"translations\"];
   mat   = d[\"distance_matrix\"];
   (* Build a fake feature vector from the distance matrix so
      Dendrogram[] can re-cluster; we just use each row as a vector
      and rely on CosineDistance which gives ~the same tree. *)
   labels = MapThread[
      Row[{#1, Spacer[6],
           Style[\"(\" <> ToString[#2] <> \")\",
                 FontSize -> 11, FontColor -> GrayLevel[0.45]]}] &,
      {names, years}];
   Dendrogram[
     MapThread[Rule, {mat, labels}],
     Left, DistanceFunction -> EuclideanDistance,
     ImageSize -> 900, AspectRatio -> 0.55,
     PlotLabel -> Style[
        \"Translation-drift phylogeny: six English NT translations on Romans 8\",
        Bold, 14, FontFamily -> \"Helvetica\"]]];

(* --- Romans 8 distance chart --------------------------------------- *)
Romans8DistChart[] := sectionDistChart[\"romans-8\", \"Romans 8\"];
Cor13DistChart[]   := sectionDistChart[\"1-corinthians-13\", \"1 Corinthians 13\"];

sectionDistChart[key_, title_] := Module[
   {data, ranked, refs, dists, plotData, topRanked, topAnnos},
   data = parseFrozen[StringReplace[key,
     {\"romans-8\" -> \"romans8translated\",
      \"1-corinthians-13\" -> \"cor13translated\"}]];
   ranked = SortBy[data, -#[\"distance\"] &];
   refs   = #[\"ref\"] & /@ data;
   dists  = #[\"distance\"] & /@ data;
   plotData = SortBy[
      MapThread[Tooltip[
         {ToExpression @ Last @ StringSplit[#1, \":\"], #2}, #1] &,
         {refs, dists}],
      First[#[[1]]] &];
   topRanked = TakeLargestBy[ranked, #[\"distance\"] &,
      Min[4, Length[ranked]]];
   topAnnos = Map[
      Function[r,
         {Black, FontFamily -> \"Helvetica\", FontSize -> 11,
          FontWeight -> Bold,
          Text[Style[r[\"ref\"], FontColor -> RGBColor[0.55, 0.15, 0.15]],
             {ToExpression @ Last @ StringSplit[r[\"ref\"], \":\"],
              r[\"distance\"]},
             {-1.1, -0.9}]}],
      topRanked];
   ListPlot[plotData,
      Filling -> Axis,
      FillingStyle -> RGBColor[0.20, 0.40, 0.65, 0.45],
      PlotStyle -> Directive[PointSize[0.013],
          RGBColor[0.15, 0.30, 0.60]],
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4],
          FontFamily -> \"Helvetica\", FontSize -> 13],
      FrameLabel -> {
         Style[\"verse\", FontFamily -> \"Helvetica\", FontSize -> 14],
         Style[\"cosine distance (KJV vs gpt-5.4)\",
            FontFamily -> \"Helvetica\", FontSize -> 14]},
      PlotLabel -> Style[
         title <> \"  \\[Dash]  per-verse divergence\",
         Bold, 16, FontFamily -> \"Helvetica\"],
      Epilog -> Flatten[topAnnos, 1],
      ImageSize -> 1200, AspectRatio -> 0.45]];

(* --- Iliad chart ---------------------------------------------------- *)
IliadDistanceMatrix[] := Module[{results, groupedBars},
   results = parseFrozen[\"iliad3way\"];
   groupedBars = Table[
      {results[[i]][\"dmat\"][[2,3]],
       results[[i]][\"dmat\"][[2,4]],
       results[[i]][\"dmat\"][[3,4]]},
      {i, Length[results]}];
   BarChart[
      Map[Tooltip[#, ToString @ NumberForm[#, {4, 3}]] &, groupedBars, {2}],
      ChartLayout -> \"Grouped\",
      ChartStyle -> {RGBColor[0.65, 0.25, 0.20], RGBColor[0.15, 0.35, 0.65],
                     RGBColor[0.25, 0.55, 0.40]},
      ChartLegends -> Placed[{
         Style[\"Pope vs Butler\",   FontFamily -> \"Helvetica\", FontSize -> 12],
         Style[\"Pope vs Modern\",   FontFamily -> \"Helvetica\", FontSize -> 12],
         Style[\"Butler vs Modern\", FontFamily -> \"Helvetica\", FontSize -> 12]},
         Right],
      ChartLabels -> {Placed[
         Style[#[\"ref\"], FontFamily -> \"Helvetica\", FontSize -> 11,
               FontColor -> GrayLevel[0.3]] & /@ results,
         Below], None},
      LabelingFunction -> (Placed[
         Style[NumberForm[#, {4, 3}], FontFamily -> \"Helvetica\",
               FontSize -> 9, FontColor -> GrayLevel[0.3]],
         Above] &),
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> \"Helvetica\",
                              FontSize -> 12],
      FrameLabel -> {None, Style[\"cosine distance (English)\",
                                 FontFamily -> \"Helvetica\", FontSize -> 14]},
      PlotLabel -> Style[\"Iliad Book 1 \\[Dash] three-way translation divergence\",
                         Bold, 15, FontFamily -> \"Helvetica\"],
      ImageSize -> 1200, AspectRatio -> 0.42]];

(* --- Tao chart ----------------------------------------------------- *)
TaoDistanceChart[] := Module[{results, groupedBars},
   results = parseFrozen[\"tao2way\"];
   groupedBars = Table[
      {results[[i]][\"d_zh_legge\"], results[[i]][\"d_zh_modern\"],
       results[[i]][\"d_legge_modern\"]},
      {i, Length[results]}];
   BarChart[groupedBars,
      ChartLayout -> \"Grouped\",
      ChartStyle -> {RGBColor[0.60, 0.25, 0.25], RGBColor[0.70, 0.40, 0.20],
                     RGBColor[0.25, 0.55, 0.40]},
      ChartLegends -> Placed[{
         Style[\"Chinese vs Legge\",         FontFamily -> \"Helvetica\", FontSize -> 12],
         Style[\"Chinese vs Modern\",         FontFamily -> \"Helvetica\", FontSize -> 12],
         Style[\"Legge vs Modern (English)\", FontFamily -> \"Helvetica\", FontSize -> 12]},
         Right],
      ChartLabels -> {Placed[
         Style[\"Ch. \" <> ToString[#[\"ch\"]], FontFamily -> \"Helvetica\",
               FontSize -> 13, FontColor -> GrayLevel[0.3]] & /@ results,
         Below], None},
      LabelingFunction -> (Placed[
         Style[NumberForm[#, {4, 3}], FontFamily -> \"Helvetica\",
               FontSize -> 9, FontColor -> GrayLevel[0.3]],
         Above] &),
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> \"Helvetica\",
                              FontSize -> 12],
      FrameLabel -> {None, Style[\"cosine distance (text-embedding-3-large)\",
                                 FontFamily -> \"Helvetica\", FontSize -> 14]},
      PlotLabel -> Style[
         \"Daodejing \\[Dash] cross-language vs within-English divergence\",
         Bold, 15, FontFamily -> \"Helvetica\"],
      ImageSize -> 1200, AspectRatio -> 0.45]];

(* --- LengthVsDivergence -------------------------------------------- *)
LengthVsDivergence[] := Module[{data, rows, lengths, dists, refs,
                                  spearman, pearson, topByDist},
   data = parseFrozen[\"romans8translated\"];
   rows = {StringLength[#[\"kjv\"]], #[\"distance\"], #[\"ref\"]} & /@ data;
   lengths = #[[1]] & /@ rows;
   dists   = #[[2]] & /@ rows;
   refs    = #[[3]] & /@ rows;
   spearman = SpearmanRho[lengths, dists];
   pearson  = Correlation[lengths, dists];
   topByDist = TakeLargestBy[
      Transpose[{lengths, dists, refs}], #[[2]] &, 4];
   ListPlot[
      MapThread[Tooltip[{#1, #2}, #3] &, {lengths, dists, refs}],
      PlotStyle -> Directive[PointSize[0.014], RGBColor[0.35, 0.20, 0.55, 0.75]],
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> \"Helvetica\",
                              FontSize -> 13],
      FrameLabel -> {
         Style[\"KJV verse length (characters)\",
               FontFamily -> \"Helvetica\", FontSize -> 14],
         Style[\"cosine distance (KJV vs GPT-5.5 modern)\",
               FontFamily -> \"Helvetica\", FontSize -> 14]},
      PlotLabel -> Style[\"Verse length vs cosine distance \\[Dash] Romans 8\",
                         Bold, 15, FontFamily -> \"Helvetica\"],
      Epilog -> {
         GrayLevel[0.3], FontFamily -> \"Helvetica\", FontSize -> 12,
         Text[StringJoin[
            \"Spearman \\[Rho] = \", ToString @ NumberForm[spearman, {4,3}],
            \"    Pearson  r = \", ToString @ NumberForm[pearson, {4,3}]],
            Scaled[{0.65, 0.93}]],
         Black, FontSize -> 10,
         Sequence @@ (Text[Style[#[[3]], FontFamily -> \"Helvetica\",
                                 FontSize -> 10, FontWeight -> Bold,
                                 FontColor -> RGBColor[0.4, 0.15, 0.15]],
                           {#[[1]], #[[2]]}, {-1.2, -0.6}] & /@ topByDist),
         GrayLevel[0.7], Dashed,
         With[{lr = Fit[Transpose[{lengths, dists}], {1, x}, x]},
           Line[{{Min[lengths], lr /. x -> Min[lengths]},
                 {Max[lengths], lr /. x -> Max[lengths]}}]]},
      ImageSize -> 1100, AspectRatio -> 0.55]];

(* --- EmbeddingModelComparison -------------------------------------- *)
EmbeddingModelComparison[] := Module[
   {d, perVerse, dLarge, dSmall, spearman, pearson, points},
   d = parseFrozen[\"embedComparison\"];
   perVerse = d[\"per_verse\"];
   dLarge = #[\"d_large\"] & /@ perVerse;
   dSmall = #[\"d_small\"] & /@ perVerse;
   spearman = d[\"spearman\"]; pearson = d[\"pearson\"];
   points = MapThread[Tooltip[{#1, #2}, #3[\"ref\"]] &,
      {dLarge, dSmall, perVerse}];
   Show[
      ListPlot[points,
         PlotStyle -> Directive[PointSize[0.012],
            RGBColor[0.20, 0.40, 0.65, 0.75]],
         Frame -> True, GridLines -> Automatic,
         FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> \"Helvetica\",
                                 FontSize -> 13],
         FrameLabel -> {
            Style[\"cosine d \\[Dash] text-embedding-3-large (3072-d)\",
               FontFamily -> \"Helvetica\", FontSize -> 14],
            Style[\"cosine d \\[Dash] text-embedding-3-small (1536-d)\",
               FontFamily -> \"Helvetica\", FontSize -> 14]},
         PlotLabel -> Style[
            \"Romans 8 \\[Dash] same KJV/modern pair, two embedding models\",
            Bold, 15, FontFamily -> \"Helvetica\"],
         PlotRange -> {{0, 0.3}, {0, 0.5}},
         AspectRatio -> 0.75, ImageSize -> 1100],
      Plot[x, {x, 0, 0.3},
         PlotStyle -> Directive[GrayLevel[0.55], Dashed, Thickness[0.0025]]],
      Epilog -> {
         GrayLevel[0.3], FontFamily -> \"Helvetica\", FontSize -> 13,
         Text[StringJoin[\"Spearman \\[Rho] = \",
               ToString @ NumberForm[spearman, {4,3}]],
            Scaled[{0.78, 0.10}]],
         Text[StringJoin[\"Pearson  r = \",
               ToString @ NumberForm[pearson, {4,3}]],
            Scaled[{0.78, 0.05}]]}]];

";

(* --- Header & footer ---------------------------------------------- *)

header = "(* ::Package:: *)

(*  LostInTranslation` \\[LongDash] consolidated Wolfram package for the
    Wolfram Community notebook \"Lost in Translation\".

    Combines what were originally several .wl and .wls files
    (framework.wl, translator_tool.wl, translation_drift.wls,
    embedding_comparison.wls, length_vs_divergence.wls, \\[Ellipsis])
    into one upload-friendly package. Every chart in the published
    notebook can be reproduced by calling a single function in here.

    Public surface (most often used):

      Audit, TranslationAuditTool   the interactive translation-audit
                                    tool from \\[Section] 9 of the post;
                                    works on any source-language passage
                                    with up to three candidate English
                                    renderings.
      DriftTimeline, DriftDendrogram   the \\[Section] 1 hook + \\[Section] 7.4 phylogeny
                                    over six PD English NT translations.
      Romans8DistChart, Cor13DistChart   per-verse distance plots from \\[Section] 4/5.
      IliadDistanceMatrix, TaoDistanceChart   the \\[Section] 6 generalisations.
      LengthVsDivergence, EmbeddingModelComparison   the \\[Section] 7 caveats.
      AvailableOpenAIModels             live model list from OpenAI.

    Setup: in any notebook, evaluate

        SetSystemCredential[\"OPENAI_API_KEY\", \"sk-\\[Ellipsis]\"]
        Get[FileNameJoin[{NotebookDirectory[], \"lost_in_translation.wl\"}]]

    and then call functions as
        LostInTranslation`DriftTimeline[]
        LostInTranslation`Audit[\"\\[Ellipsis]source\\[Ellipsis]\", \"Spanish\",
           \"\\[Ellipsis]candidate 1\\[Ellipsis]\", \"\\[Ellipsis]candidate 2\\[Ellipsis]\"]
*)

BeginPackage[\"LostInTranslation`\"];

(* Public symbols (a short selection \\[Dash] all the functions used in the
   notebook code cells). *)
OpenAIKey::usage          = \"OpenAIKey[] returns the API key from SystemCredential.\";
AvailableOpenAIModels::usage = \"AvailableOpenAIModels[] returns the live list of OpenAI chat-capable models.\";
LLMTranslate::usage       = \"LLMTranslate[text, ref, model] returns a careful modern English translation.\";

Audit::usage              = \"Audit[source, sourceLang, candidates..., Model -> name] runs a full translation-pair audit and returns a formatted Column[].\";
RunAudit::usage           = \"RunAudit[source, sourceLang, candidates] returns the audit Association.\";
RunAuditProgressive::usage = \"RunAuditProgressive[source, sourceLang, candidates, statusFn] is RunAudit with a per-stage callback.\";
TranslationAuditTool::usage = \"TranslationAuditTool[] returns the interactive translator-audit DynamicModule.\";
Model::usage              = \"Model is an option for Audit / RunAudit / RunAuditProgressive.\";

DriftTimeline::usage           = \"DriftTimeline[] returns the cumulative-drift line plot (\\[Section] 1).\";
DriftDendrogram::usage         = \"DriftDendrogram[] returns the translation-drift phylogeny (\\[Section] 7.4).\";
Romans8DistChart::usage        = \"Romans8DistChart[] returns the Romans 8 per-verse distance plot.\";
Cor13DistChart::usage          = \"Cor13DistChart[] returns the 1 Corinthians 13 per-verse distance plot.\";
IliadDistanceMatrix::usage     = \"IliadDistanceMatrix[] returns the Iliad three-way bar chart.\";
TaoDistanceChart::usage        = \"TaoDistanceChart[] returns the Daodejing distance chart.\";
LengthVsDivergence::usage      = \"LengthVsDivergence[] returns the verse-length-vs-divergence scatter.\";
EmbeddingModelComparison::usage = \"EmbeddingModelComparison[] returns the large-vs-small embedding scatter.\";

Begin[\"`Private`\"];
";

footer = "

End[];
EndPackage[];
";

content = header <> "\n\n(* === framework.wl body === *)\n\n" <> fwBody <>
   "\n\n(* === translator_tool.wl body === *)\n\n" <> ttBody <>
   "\n\n(* === chart-rendering functions === *)\n\n" <> chartFunctionsBody <>
   "\n\n" <> encodedFrozen <>
   footer;

Export[out, content, "Text"];
Print["wrote ", out, " (", Round[FileByteCount[out]/1024], " KB)"];
