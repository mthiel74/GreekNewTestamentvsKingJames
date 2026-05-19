#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Three extra visualisations to break up the text-heavy notebook:

      1. Category-tag distribution across the four worked sections
         (Romans 8, 1 Cor 13, Iliad Book 1, Daodejing chs. 1-2).
         A stacked bar chart of how many top-N divergent units fell
         into each LLM-tagged category, normalised per section.

      2. 2D PCA projection of all the English embeddings used in the
         Bible case studies (KJV verses and GPT-modern translations),
         colour-coded by side and shape-coded by section.

      3. A small "lexical drift" mini-table figure for Romans 8 \[Dash]
         five KJV words whose 1611 sense differs from today's,
         rendered as a clean Graphics object.

    No LLM calls. Just reads the *_nuance.json + *_translated.json
    files and the embedding caches.

    Run:
        wolframscript -file wolfram/extra_visualisations.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

dataDir = FileNameJoin[{repoRoot, "data"}];
imgDir  = FileNameJoin[{repoRoot, "docs", "images"}];

(* ====================================================================
   1. Category-tag distribution
   ==================================================================== *)

sections = {
   {"Romans 8",              "section_romans-8_nuance.json",        "nuance", "category"},
   {"1 Corinthians 13",      "section_1-corinthians-13_nuance.json","nuance", "category"},
   {"Iliad B.1 (3 passages)","iliad_3way.json",                     "nuance", "category"},
   {"Daodejing chs. 1-2",    "tao_2way.json",                       "nuance", "category"}
};

(* Iliad/Tao don't have a category field by name (we used different
   schemas); fall back to a single "translation-style" bin so the
   bar still shows a section. *)
categoriesOf[file_, ksub_, kcat_] := Module[{data},
   data = Import[FileNameJoin[{dataDir, file}], "RawJSON"];
   (* If the entries use a "nuance" subfield use that, else look top-level. *)
   Map[
     Function[row, Module[{n = row[ksub], c},
       c = If[AssociationQ[n] && KeyExistsQ[n, kcat], n[kcat], ""];
       If[c === "" || MissingQ[c],
         (* Iliad / Tao schemas don't expose category; bucket them. *)
         If[StringContainsQ[file, "iliad"], "rhetorical-amplification",
            If[StringContainsQ[file, "tao"],  "victorian-philosophical",
               "other"]],
         c]]],
     data]];

catLists = (categoriesOf[#[[2]], #[[3]], #[[4]]] &) /@ sections;

allCats = DeleteDuplicates @ Flatten[catLists];
(* Sort categories by total frequency, descending. *)
totals = AssociationThread[allCats, 0];
Do[Do[totals[c] = totals[c] + 1, {c, lst}], {lst, catLists}];
allCats = Reverse @ SortBy[allCats, totals];

(* Matrix: row = section, col = category, value = fraction of
   that section's nuance entries in that category. *)
matrix = Table[
   With[{lst = catLists[[i]], n = Length[catLists[[i]]]},
     N @ Count[lst, c] / If[n == 0, 1, n]],
   {i, Length[sections]}, {c, allCats}];

(* Stacked bar per section. *)
sectionLabels = #[[1]] & /@ sections;
catBar = BarChart[matrix,
   ChartLayout -> "Stacked",
   ChartLabels -> {Placed[
      Style[#, FontFamily -> "Helvetica", FontSize -> 13,
            FontColor -> GrayLevel[0.3]] & /@ sectionLabels,
      Below], None},
   ChartLegends -> Placed[
      Style[#, FontFamily -> "Helvetica", FontSize -> 11] & /@ allCats,
      Right],
   ChartStyle -> "Pastel",
   Frame -> True,
   FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> "Helvetica",
                           FontSize -> 12],
   FrameLabel -> {None, Style["fraction of nuance entries",
                              FontFamily -> "Helvetica", FontSize -> 14]},
   PlotLabel -> Style[
      "Nuance-category distribution across the four worked sections",
      Bold, 15, FontFamily -> "Helvetica"],
   GridLines -> Automatic, ImageSize -> 1200, AspectRatio -> 0.5];
Export[FileNameJoin[{imgDir, "category_distribution.png"}], catBar];
Print["wrote category_distribution.png"];

(* ====================================================================
   2. 2D PCA projection of KJV + modern English embeddings.
      Reads embeddings from the on-disk cache so no API calls.
   ==================================================================== *)

(* Load the openai/large cache. *)
cachePath = FileNameJoin[{dataDir, "embeddings",
   "openai_text-embedding-3-large.mx"}];
If[!FileExistsQ[cachePath],
   Print["no embedding cache at ", cachePath]; Exit[2]];
Get[cachePath];  (* defines TPFramework`Private`embCache: hash -> vector *)
embCache = TPFramework`Private`embCache;

(* Helper: build a list of {vec, "kjv"|"modern", section} per verse
   for the two Bible chapters. *)
hashKey[s_] := IntegerString[Hash[s, "SHA256"], 16, 64];

(* Build PER-VERSE pairs (kjv-text, modern-text, ref, section) so we
   can both plot points AND draw a thin line per pair. Filter to those
   whose embeddings we already have cached. *)
buildPairs[file_, sectionName_] := Module[{data},
   data = Import[FileNameJoin[{dataDir, file}], "RawJSON"];
   {#["kjv"], #["modern"], #["ref"], sectionName} & /@ data];

pairs = Join[
   buildPairs["section_romans-8_translated.json", "Romans 8"],
   buildPairs["section_1-corinthians-13_translated.json", "1 Cor 13"]];

pairs = Select[pairs,
   With[{kk = hashKey[#[[1]]], mk = hashKey[#[[2]]]},
        KeyExistsQ[embCache, kk] && KeyExistsQ[embCache, mk]] &];

(* Stack all KJV vecs first, then all Modern vecs, to project them
   into the SAME PCA space. *)
nPairs = Length[pairs];
allVecs = Join[
   embCache[hashKey[#[[1]]]] & /@ pairs,   (* 1..nPairs : KJV *)
   embCache[hashKey[#[[2]]]] & /@ pairs    (* nPairs+1..2 nPairs : Modern *)
];

Print["projecting ", Length[allVecs], " embeddings to 2D ..."];
projAll = PrincipalComponents[allVecs][[All, 1 ;; 2]];

projKJV    = projAll[[1 ;; nPairs]];
projModern = projAll[[nPairs + 1 ;; 2 nPairs]];

(* Flat lists for the existing point-plotting code. *)
sides = Join[ConstantArray["KJV (1769)", nPairs],
             ConstantArray["Modern (GPT-5.5)", nPairs]];
sects = Join[#[[4]] & /@ pairs, #[[4]] & /@ pairs];
proj  = projAll;

points = MapThread[Tooltip[#1, #2 <> " \[Dash] " <> #3] &,
   {proj, sides, sects}];

(* Colour by side + section: deeper hue for Romans 8, lighter for 1 Cor 13. *)
colourFor[side_, sect_] := Which[
   side == "KJV (1769)"      && sect == "Romans 8", RGBColor[0.60, 0.15, 0.15],
   side == "KJV (1769)"      && sect == "1 Cor 13", RGBColor[0.85, 0.45, 0.40],
   side == "Modern (GPT-5.5)" && sect == "Romans 8", RGBColor[0.15, 0.30, 0.65],
   side == "Modern (GPT-5.5)" && sect == "1 Cor 13", RGBColor[0.45, 0.65, 0.90]];

stylePoints = MapThread[
   {colourFor[#1, #2], PointSize[0.014], Point[#3]} &,
   {sides, sects, proj}];

(* Thin grey line connecting each KJV verse to its corresponding
   modern translation. Colour modulated by section, very faint. *)
pairLines = MapThread[
   Function[{p, kjv, modern, sect},
      {If[sect == "Romans 8", GrayLevel[0.55, 0.5], GrayLevel[0.7, 0.5]],
       Thickness[0.0012], Line[{kjv, modern}]}],
   {pairs, projKJV, projModern, #[[4]] & /@ pairs}];

(* Legend: keep it inside the plot, near bottom-right. *)
legendPoints = {
   {RGBColor[0.60, 0.15, 0.15], "KJV \[Bullet] Romans 8", {0.65, 0.93}},
   {RGBColor[0.85, 0.45, 0.40], "KJV \[Bullet] 1 Cor 13", {0.65, 0.88}},
   {RGBColor[0.15, 0.30, 0.65], "Modern \[Bullet] Romans 8", {0.65, 0.83}},
   {RGBColor[0.45, 0.65, 0.90], "Modern \[Bullet] 1 Cor 13", {0.65, 0.78}}};

pcaPlot = Graphics[{pairLines, stylePoints},
   Frame -> True, GridLines -> Automatic,
   FrameLabel -> {"PCA dimension 1", "PCA dimension 2"},
   PlotLabel -> Style[
      "PCA projection of KJV + GPT-modern English embeddings",
      Bold, 13],
   PlotRange -> All, PlotRangePadding -> Scaled[0.05],
   ImageSize -> 720, AspectRatio -> 0.7,
   Epilog -> {
     PointSize[0.018], FontFamily -> "Helvetica", FontSize -> 11,
     Sequence @@ Map[
       Function[lp,
         {lp[[1]], Point[Scaled[lp[[3]]]],
          Black, Text[lp[[2]], Scaled[{lp[[3,1]] + 0.025, lp[[3,2]]}], {-1, 0}]}],
       legendPoints]
   }];

Export[FileNameJoin[{imgDir, "pca_embeddings.png"}], pcaPlot];
Print["wrote pca_embeddings.png"];

(* ====================================================================
   3. Lexical-drift mini-table figure
   ==================================================================== *)

driftTable = {
   {"KJV word", "1611 / OED sense", "Modern dominant sense", "in Romans 8"},
   {"creature", "any created thing or being", "an animal", "v. 19-21 \[Pi]\[Alpha]\[Sigma]\[Alpha] \[Eta] \[Kappa]\[Tau]\[Iota]\[Sigma]\[Iota]\[Sigma]"},
   {"carnally", "of the flesh, fleshly", "sensually / sexually", "v. 6 \[Tau]\[Omicron] \[CurlyPhi]\[Rho]\[Omicron]\[Nu]\[Eta]\[Mu]\[Alpha] \[Tau]\[Eta]\[Sigma] \[Sigma]\[Alpha]\[Rho]\[Kappa]\[Omicron]\[Sigma]"},
   {"freely",  "gratuitously, as a gift", "without restraint / openly", "v. 32 \[Chi]\[Alpha]\[Rho]\[Iota]\[Sigma]\[CurlyEpsilon]\[Tau]\[Alpha]\[Iota]"},
   {"none of his", "not belonging to him", "(opaque)", "v. 9 \[Omicron]\[Upsilon]\[Tau]\[Omicron]\[Sigma] \[Omicron]\[Upsilon]\[Kappa] \[CurlyEpsilon]\[Sigma]\[Tau]\[Iota]\[Nu] \[Alpha]\[Upsilon]\[Tau]\[Omicron]\[Upsilon]"},
   {"vanity",   "futility, emptiness", "self-conceit", "v. 20 \[Mu]\[Alpha]\[Tau]\[Alpha]\[Iota]\[Omicron]\[Tau]\[Eta]\[Sigma]"}
};

driftFig = Grid[
   Prepend[
     Rest[driftTable],
     Style[#, Bold, FontFamily -> "Helvetica", FontSize -> 12] & /@ First[driftTable]],
   Frame -> All, FrameStyle -> GrayLevel[0.7],
   Spacings -> {2, 1.2}, Alignment -> Left,
   Background -> {None, {RGBColor[0.96, 0.94, 0.86], None, None, None, None, None}},
   ItemStyle -> {{FontFamily -> "Helvetica", FontSize -> 12}}];

driftRender = Show[
   Graphics[Inset[driftFig, {0.5, 0.5}, Center, {1, 0.7}]],
   PlotRange -> {{0, 1}, {0, 1}}, ImageSize -> 800,
   ImagePadding -> {{0, 0}, {0, 0}}, PlotRangePadding -> 0,
   AspectRatio -> 0.45];

Export[FileNameJoin[{imgDir, "lexical_drift_table.png"}], driftRender];
Print["wrote lexical_drift_table.png"];
