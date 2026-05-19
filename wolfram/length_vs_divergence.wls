#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Length-vs-divergence scatter: an honest-caveat plot for \[Section] 7.

    Does the framework over-detect long verses? Plot KJV-character-length
    vs cosine-distance for every Romans 8 verse, report Spearman correlation,
    annotate the outliers.

    Output: docs/images/length_vs_divergence.png
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

data = Import[FileNameJoin[{repoRoot, "data",
   "section_romans-8_translated.json"}], "RawJSON"];

(* Per verse: KJV length in characters, distance, ref *)
rows = {StringLength[#["kjv"]], #["distance"], #["ref"]} & /@ data;
lengths = #[[1]] & /@ rows;
dists   = #[[2]] & /@ rows;
refs    = #[[3]] & /@ rows;

(* Spearman + Pearson *)
spearman = SpearmanRho[lengths, dists];
pearson  = Correlation[lengths, dists];
Print["Spearman rho: ", NumberForm[spearman, {4, 3}]];
Print["Pearson  r:   ", NumberForm[pearson, {4, 3}]];

(* Identify which verses are MOST and LEAST representative of the
   length\[Dash]divergence trend. The annotated callouts make the chart
   readable without a separate table. *)
(* Top 4 by distance get labels. *)
topByDist = TakeLargestBy[
   Transpose[{lengths, dists, refs}], #[[2]] &, 4];

scatter = ListPlot[
   MapThread[Tooltip[{#1, #2}, #3] &, {lengths, dists, refs}],
   PlotStyle -> Directive[PointSize[0.014], RGBColor[0.35, 0.20, 0.55, 0.75]],
   Frame -> True, GridLines -> Automatic,
   FrameLabel -> {
      Style["KJV verse length (characters)",
            FontFamily -> "Helvetica", FontSize -> 14],
      Style["cosine distance (KJV vs GPT-5.5 modern)",
            FontFamily -> "Helvetica", FontSize -> 14]},
   PlotLabel -> Style[
      "Verse length vs cosine distance \[Dash] Romans 8",
      Bold, 15, FontFamily -> "Helvetica"],
   Epilog -> {
      (* Spearman annotation *)
      GrayLevel[0.3], FontFamily -> "Helvetica", FontSize -> 12,
      Text[Style[StringJoin[
         "Spearman \[Rho] = ", ToString @ NumberForm[spearman, {4,3}],
         "    Pearson  r = ", ToString @ NumberForm[pearson, {4,3}]],
         GrayLevel[0.3]],
         Scaled[{0.65, 0.93}]],
      (* Top-4 outlier labels *)
      Black, FontSize -> 10,
      Sequence @@ (Text[Style[#[[3]], FontFamily -> "Helvetica",
                              FontSize -> 10, FontWeight -> Bold,
                              FontColor -> RGBColor[0.4, 0.15, 0.15]],
                        {#[[1]], #[[2]]}, {-1.2, -0.6}] & /@ topByDist),
      (* Light-grey best-fit line *)
      GrayLevel[0.7], Dashed,
      With[{lr = Fit[Transpose[{lengths, dists}], {1, x}, x]},
        Line[{{Min[lengths], lr /. x -> Min[lengths]},
              {Max[lengths], lr /. x -> Max[lengths]}}]]},
   ImageSize -> 1100, AspectRatio -> 0.55];

Export[FileNameJoin[{imgDir, "length_vs_divergence.png"}], scatter];
Print["wrote length_vs_divergence.png"];

(* Save the numerical result to JSON for the notebook builder. *)
Export[FileNameJoin[{repoRoot, "data", "length_vs_divergence.json"}],
   <|"spearman" -> spearman, "pearson" -> pearson,
     "n_verses" -> Length[rows],
     "median_length" -> Median[lengths],
     "median_distance" -> Median[dists]|>,
   "JSON"];
Print["wrote length_vs_divergence.json"];
