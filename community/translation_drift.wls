#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Translation-drift analyses on the six historical PD English NT
    translations of Romans 8.

    Produces:
       data/translation_drift.json   - distance matrix + verse-by-verse
                                       cumulative drift
       docs/images/drift_phylogeny.png   - Dendrogram[]
       docs/images/drift_timeline.png    - cumulative-drift line plot

    Uses the same on-disk embedding cache as the rest of the pipeline,
    so re-runs are free after the first fetch.

    Run:
        wolframscript -file wolfram/translation_drift.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

dataDir = FileNameJoin[{repoRoot, "data"}];
imgDir  = FileNameJoin[{repoRoot, "docs", "images"}];

data = Import[FileNameJoin[{dataDir, "historical_bibles_romans8.json"}],
   "RawJSON"];

(* Sort by year to give the drift chart a natural left-to-right axis. *)
data = SortBy[data, #["year"] &];
names = #["short"] & /@ data;
years = #["year"] & /@ data;
nT = Length[data];

(* Verses present in ALL six translations. The regex parser misses a
   few WEB verses with embedded footnotes; we restrict to the common
   set so every pairwise comparison is apples-to-apples. *)
commonVerses = Select[Range[39],
   Function[n, AllTrue[data, KeyExistsQ[#["verses"], ToString[n]] &]]];
Print["Common verses across all ", nT, " translations: ",
      Length[commonVerses], " (", First[commonVerses], "\[Dash]", Last[commonVerses], ")"];

(* Per-translation: concatenate the common verses into one string, then
   embed once. Embedding the chapter as a whole is sufficient for the
   distance matrix; verse-level lookups come later for the timeline. *)
concatenated = Map[
   Function[t,
     StringRiffle[Lookup[t["verses"], ToString /@ commonVerses], " "]],
   data];

Print["embedding ", nT, " translation chapters with text-embedding-3-large \[Ellipsis]"];
vecs = EmbedTexts[concatenated, EmbeddingProvider -> "openai"];
If[vecs === $Failed, Print["embedding failed"]; Exit[2]];

(* Pairwise cosine distance matrix. *)
distMat = Table[CosineDistance[vecs[[i]], vecs[[j]]],
   {i, nT}, {j, nT}];
Print["pairwise distances:"];
Print[Grid[
   Prepend[
     MapThread[Prepend, {
        Map[NumberForm[#, {4,3}] &, distMat, {2}], names}],
     Prepend[names, ""]],
   Frame -> All]];

(* --- DENDROGRAM ---------------------------------------------------- *)
(* Dendrogram[] takes labeled vectors and builds a hierarchical tree
   on the supplied distance function. *)
(* The rule form `vec -> name` makes Dendrogram label leaves with the
   name only; the vector itself is the keyed clustering input.  Labeled[]
   makes Dendrogram render BOTH the vec and the name as leaf text \[Dash]
   that produced a 13 MB PNG of vector noise. *)
labels = MapThread[
   Row[{#1, Spacer[6], Style["(" <> ToString[#2] <> ")",
        FontSize -> 11, FontColor -> GrayLevel[0.45]]}] &,
   {names, years}];
tree = Dendrogram[
   MapThread[Rule, {vecs, labels}],
   Left, DistanceFunction -> CosineDistance,
   ImageSize -> 900, AspectRatio -> 0.55,
   PlotLabel -> Style[
      "Translation-drift phylogeny: six English NT translations on Romans 8",
      Bold, 14, FontFamily -> "Helvetica"]];
Export[FileNameJoin[{imgDir, "drift_phylogeny.png"}], tree];
Print["wrote drift_phylogeny.png"];

(* --- TIMELINE ------------------------------------------------------- *)
(* Cumulative drift over time: distance from the earliest translation
   (Tyndale 1534) to each successive translation. Shows the language
   shift accumulating across four and a half centuries. *)
driftFromTyndale = distMat[[1, ;;]];
pts = MapThread[
   Callout[{#1, #2}, #3 <> " (" <> ToString[#1] <> ")",
           Above, LabelStyle -> {FontSize -> 11}] &,
   {years, driftFromTyndale, names}];

timeline = ListLinePlot[
   Style[Tooltip[#[[1]], #[[2]]] & /@ Transpose[{Transpose[{years, driftFromTyndale}], names}], Thickness[0.004]],
   PlotStyle -> Directive[RGBColor[0.55, 0.20, 0.15], Thickness[0.004]],
   PlotMarkers -> {Automatic, 10},
   Epilog -> MapThread[
     {Black, Disk[{#1, #2}, Offset[5]],
      Text[Style[#3 <> "\n" <> ToString[#1],
            FontFamily -> "Helvetica", FontSize -> 10,
            FontColor -> GrayLevel[0.25]],
           {#1, #2}, {0, -1.4}]} &,
     {years, driftFromTyndale, names}],
   Frame -> True, GridLines -> Automatic,
   FrameLabel -> {Style["year", FontFamily -> "Helvetica", FontSize -> 13],
                  Style["cosine distance from Tyndale 1534",
                        FontFamily -> "Helvetica", FontSize -> 13]},
   PlotLabel -> Style[
      "Cumulative translation drift: Romans 8, Tyndale 1534 baseline",
      Bold, 14, FontFamily -> "Helvetica"],
   ImageSize -> 1100, AspectRatio -> 0.5,
   PlotRange -> {{1525, 2020}, {0, 1.15 * Max[driftFromTyndale]}}];
Export[FileNameJoin[{imgDir, "drift_timeline.png"}], timeline];
Print["wrote drift_timeline.png"];

(* --- save JSON --- *)
out = <|
   "translations" -> MapThread[<|"short" -> #1, "year" -> #2|> &,
      {names, years}],
   "verses_common" -> commonVerses,
   "distance_matrix" -> distMat,
   "drift_from_baseline" -> AssociationThread[names, driftFromTyndale]
|>;
Export[FileNameJoin[{dataDir, "translation_drift.json"}], out, "JSON"];
Print["wrote translation_drift.json"];
