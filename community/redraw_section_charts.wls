#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Regenerate the per-section distance plots from existing
    *_translated.json files \[Dash] no API calls.

    Picks up the polished plot styling from section_analysis.wls's
    distance figure (high-res, Helvetica, top-4 annotated). *)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];

sectionKeys = {"romans-8", "1-corinthians-13"};
titleFor    = <|"romans-8" -> "Romans 8",
                 "1-corinthians-13" -> "1 Corinthians 13"|>;

redraw[key_String] := Module[
   {data, ranked, refs, dists, plotData, topRanked, topAnnos, distPlot},
   data = Import[FileNameJoin[{repoRoot, "data",
      "section_" <> key <> "_translated.json"}], "RawJSON"];
   ranked = SortBy[data, -#["distance"] &];
   refs = #["ref"] & /@ data;
   dists = #["distance"] & /@ data;
   plotData = SortBy[
      MapThread[Tooltip[
         {ToExpression @ Last @ StringSplit[#1, ":"], #2}, #1] &,
         {refs, dists}],
      First[#[[1]]] &];
   topRanked = TakeLargestBy[ranked, #["distance"] &,
      Min[4, Length[ranked]]];
   topAnnos = Map[
      Function[r,
         {Black, FontFamily -> "Helvetica", FontSize -> 11,
          FontWeight -> Bold,
          Text[Style[r["ref"], FontColor -> RGBColor[0.55, 0.15, 0.15]],
             {ToExpression @ Last @ StringSplit[r["ref"], ":"],
              r["distance"]},
             {-1.1, -0.9}]}],
      topRanked];
   distPlot = ListPlot[plotData,
      Filling -> Axis,
      FillingStyle -> RGBColor[0.20, 0.40, 0.65, 0.45],
      PlotStyle -> Directive[PointSize[0.013],
          RGBColor[0.15, 0.30, 0.60]],
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4],
          FontFamily -> "Helvetica", FontSize -> 13],
      FrameLabel -> {
         Style["verse", FontFamily -> "Helvetica", FontSize -> 14],
         Style["cosine distance (KJV vs gpt-5.4)",
            FontFamily -> "Helvetica", FontSize -> 14]},
      PlotLabel -> Style[
         titleFor[key] <> "  \[Dash]  per-verse divergence",
         Bold, 16, FontFamily -> "Helvetica"],
      Epilog -> Flatten[topAnnos, 1],
      ImageSize -> 1200, AspectRatio -> 0.45];
   Export[FileNameJoin[{imgDir, "section_" <> key <> "_distance.png"}],
      distPlot];
   Print["  wrote section_", key, "_distance.png  (",
         Length[data], " verses, top peak at d=",
         NumberForm[First[ranked]["distance"], {4, 3}], ")"]];

Do[redraw[k], {k, sectionKeys}];
