#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Real, end-to-end analysis of a few hand-picked NT sections —
    not the whole corpus. Workflow per verse:

      1. Read Koine Greek (SBLGNT) and KJV from the canonical JSON.
      2. Ask GPT-5.5 (reasoning_effort=high) for a careful modern
         English translation of the Greek.
      3. Embed the KJV and the modern translation (both English)
         with text-embedding-3-large. Same-language embedding is
         a clean semantic-distance signal — the cross-lingual
         comparison the embedder does poorly is sidestepped.
      4. Rank the verses in the section by cosine distance.
      5. For the top-N divergent verses, ask GPT-5.5 to articulate
         the *nuance* — small, real differences that a casual
         reader misses (theological loading, archaic English,
         manuscript variants, idiom handling, ...). Skips the
         obvious "thou/you" stuff per the user's note.

    Outputs:
       data/section_<key>_translated.json   (verse → kjv, modern, distance)
       data/section_<key>_nuance.json       (top-N rows + LLM nuance)
       docs/images/section_<key>_distance.png

    Sections covered: Romans 8 (39 verses, theologically dense),
    John 1 (51 verses, the prologue), 1 Corinthians 13 (13 verses,
    "the love chapter"). Run with --section to pick one.

    Usage:
       wolframscript -file wolfram/section_analysis.wls -- romans-8
       wolframscript -file wolfram/section_analysis.wls -- all
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

(* --- section registry --- *)
sections = <|
   "romans-8"        -> <|"book" -> "Romans",        "abbr" -> "Ro",  "ch" -> 8,  "title" -> "Romans 8"|>,
   "john-1"          -> <|"book" -> "John",          "abbr" -> "Jn",  "ch" -> 1,  "title" -> "John 1"|>,
   "1-corinthians-13"-> <|"book" -> "1 Corinthians", "abbr" -> "1Co", "ch" -> 13, "title" -> "1 Corinthians 13"|>
|>;

(* --- args --- *)
args = DeleteCases[Rest[$ScriptCommandLine], "--"];
target = If[Length[args] >= 1, args[[1]], "romans-8"];
topN = If[Length[args] >= 2, ToExpression[args[[2]]], 10];

If[!KeyExistsQ[sections, target] && target =!= "all",
   Print["unknown section: ", target];
   Print["known: ", Keys[sections], " or 'all'"];
   Exit[2]];

todo = If[target === "all", Keys[sections], {target}];

(* --- load corpora once --- *)
gnt = Import[FileNameJoin[{repoRoot, "data", "greek_nt.json"}], "RawJSON"];
kjv = Import[FileNameJoin[{repoRoot, "data", "kjv_nt.json"}],   "RawJSON"];

(* Strip morphgnt text-critical marks (⸀ ⸂ ⸃ ⸀ ⸁ etc) before sending
   to GPT — they confuse the model and aren't part of the running text. *)
stripMarks[s_String] := StringDelete[s, RegularExpression["[\\x{2E00}-\\x{2E7F}]"]];

dataDir = FileNameJoin[{repoRoot, "data"}];
imgDir  = FileNameJoin[{repoRoot, "docs", "images"}];

processSection[key_String] := Module[
   {meta, sectionG, sectionK, refs, greeks, kjvs, modern, dists, ranked,
    nuanced, out, plotData, distPlot, outBase},
   meta = sections[key];
   Print["\n=========================================="];
   Print["  Section: ", meta["title"]];
   Print["==========================================\n"];
   sectionG = Select[gnt, #["abbr"] == meta["abbr"] && #["ch"] == meta["ch"] &];
   sectionK = Select[kjv, #["abbr"] == meta["abbr"] && #["ch"] == meta["ch"] &];
   refs = SortBy[#["ref"] & /@ sectionG, ToExpression @ Last @ StringSplit[#, ":"] &];
   Print["  ", Length[refs], " verses on the Greek side, ",
         Length[sectionK], " on the KJV side"];

   (* --- step 1+2: translate each Greek verse with GPT-5.5 thinking. --- *)
   Print["\n[1/3] translating verses with GPT-5.5 (reasoning_effort=high) ..."];
   modern = <||>;
   greeks = <||>;
   kjvs   = <||>;
   Do[Module[{gRow, kRow, gText, kText, mText},
      gRow = SelectFirst[sectionG, #["ref"] == ref &];
      kRow = SelectFirst[sectionK, #["ref"] == ref &];
      If[MissingQ[gRow] || MissingQ[kRow], Continue[]];
      gText = stripMarks @ gRow["text"];
      kText = kRow["text"];
      greeks[ref] = gText;
      kjvs[ref]   = kText;
      Print["  ", ref, "  translating ..."];
      mText = LLMTranslate[gText, ref];
      If[mText === $Failed,
        Print["    [translation failed; skipping this verse]"]; Continue[]];
      modern[ref] = mText;
   ], {ref, refs}];

   Print["  translated ", Length[modern], " of ", Length[refs], " verses"];

   (* --- step 3: embed KJV and modern; cosine distance. --- *)
   Print["\n[2/3] embedding KJV + modern translations (text-embedding-3-large) ..."];
   refs = Select[refs, KeyExistsQ[modern, #] &]; (* drop failed ones *)
   {embK, embM} = {
      EmbedTexts[Lookup[kjvs, refs],   EmbeddingProvider -> "openai"],
      EmbedTexts[Lookup[modern, refs], EmbeddingProvider -> "openai"]};
   If[embK === $Failed || embM === $Failed,
     Print["embedding failed"]; Return[$Failed]];
   dists = MapThread[CosineDistance, {embK, embM}];

   ranked = SortBy[
      MapThread[<|
         "ref" -> #1, "greek" -> greeks[#1], "kjv" -> kjvs[#1],
         "modern" -> modern[#1], "distance" -> #2|> &,
         {refs, dists}],
      -#["distance"] &];
   outBase = FileNameJoin[{dataDir, "section_" <> key}];
   Export[outBase <> "_translated.json", ranked, "JSON"];
   Print["  wrote ", outBase, "_translated.json"];

   (* --- distance figure ---
      High-res (1200 px wide), Helvetica typography, top-4 verses
      labelled with their ref so the reader doesn't have to consult the
      table to know which peaks are which. *)
   plotData = SortBy[
      MapThread[Tooltip[
         {ToExpression @ Last @ StringSplit[#1, ":"], #2}, #1] &,
         {refs, dists}],
      First[#[[1]]] &];
   Module[{topRanked, topAnnos},
     topRanked = TakeLargestBy[ranked, #["distance"] &,
        Min[4, Length[ranked]]];
     topAnnos = Map[
        Function[r,
           {Black, FontFamily -> "Helvetica", FontSize -> 10,
            FontWeight -> Bold,
            Text[Style[r["ref"], FontColor -> RGBColor[0.55, 0.15, 0.15]],
               {ToExpression @ Last @ StringSplit[r["ref"], ":"],
                r["distance"]},
               {-1.1, -0.8}]}],
        topRanked];
     distPlot = ListPlot[plotData,
        Filling -> Axis,
        FillingStyle -> RGBColor[0.20, 0.40, 0.65, 0.45],
        PlotStyle -> Directive[PointSize[0.011],
            RGBColor[0.15, 0.30, 0.60]],
        Frame -> True, GridLines -> Automatic,
        FrameStyle -> Directive[GrayLevel[0.4],
            FontFamily -> "Helvetica", FontSize -> 13],
        FrameLabel -> {
           Style["verse", FontFamily -> "Helvetica", FontSize -> 14],
           Style["cosine distance (KJV vs " <> "gpt-5.4" <> ")",
              FontFamily -> "Helvetica", FontSize -> 14]},
        PlotLabel -> Style[
           meta["title"] <> "  \[Dash]  per-verse divergence",
           Bold, 16, FontFamily -> "Helvetica"],
        Epilog -> Flatten[topAnnos, 1],
        ImageSize -> 1200, AspectRatio -> 0.45]];
   Export[FileNameJoin[{imgDir, "section_" <> key <> "_distance.png"}], distPlot];
   Print["  wrote section_", key, "_distance.png"];

   (* --- step 4: nuance analysis on top-N divergent verses --- *)
   Print["\n[3/3] nuance analysis on top ", topN, " verses (GPT-5.5 thinking) ..."];
   nuanced = MapIndexed[
      (Print["  ", First[#2], "/", Min[topN, Length[ranked]], "  ",
             #1["ref"], "  d=", NumberForm[#1["distance"], {4, 3}]];
       LLMNuanceCompare[#1]) &,
      Take[ranked, UpTo[topN]]];
   nuanced = Select[nuanced, AssociationQ];
   Export[outBase <> "_nuance.json", nuanced, "JSON"];
   Print["  wrote ", outBase, "_nuance.json"];

   <|"section" -> key, "nVerses" -> Length[ranked],
     "nNuanced" -> Length[nuanced]|>
];

results = processSection /@ todo;

Print["\n=== done ==="];
Print[results];
