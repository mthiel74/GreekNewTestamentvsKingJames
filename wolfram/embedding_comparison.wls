#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Compare two OpenAI embedding models on the Romans 8 worked
    case study: text-embedding-3-large (3072-d, the model the rest
    of the project uses) vs text-embedding-3-small (1536-d, cheaper).

    For each of the 39 verses we have already produced (KJV + GPT
    modern) the cosine distance with both models. We then quantify
    the disagreement:
       \[Bullet] Spearman rank correlation across the two rankings
       \[Bullet] scatter plot of small-d vs large-d per verse
       \[Bullet] the verses where the two models differ most in rank

    Output:
        data/embedding_comparison.json
        docs/images/embedding_comparison_scatter.png

    Cost: 39*2 = 78 embedding calls in each model; about $0.005 total.

    Run:
        wolframscript -file wolfram/embedding_comparison.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

dataDir = FileNameJoin[{repoRoot, "data"}];
imgDir  = FileNameJoin[{repoRoot, "docs", "images"}];

(* Load the existing Romans 8 translations. *)
romans = Import[FileNameJoin[{dataDir, "section_romans-8_translated.json"}],
   "RawJSON"];
Print["loaded ", Length[romans], " Romans 8 rows"];

kjvTexts    = #["kjv"]    & /@ romans;
modernTexts = #["modern"] & /@ romans;

Print["\nEmbedding with text-embedding-3-large (3072-d) ..."];
embKlarge = EmbedTexts[kjvTexts,
   EmbeddingProvider -> "openai", EmbeddingModel -> "text-embedding-3-large"];
embMlarge = EmbedTexts[modernTexts,
   EmbeddingProvider -> "openai", EmbeddingModel -> "text-embedding-3-large"];

Print["\nEmbedding with text-embedding-3-small (1536-d) ..."];
embKsmall = EmbedTexts[kjvTexts,
   EmbeddingProvider -> "openai", EmbeddingModel -> "text-embedding-3-small"];
embMsmall = EmbedTexts[modernTexts,
   EmbeddingProvider -> "openai", EmbeddingModel -> "text-embedding-3-small"];

dLarge = MapThread[CosineDistance, {embKlarge, embMlarge}];
dSmall = MapThread[CosineDistance, {embKsmall, embMsmall}];

Print["\n--- summary statistics ---"];
Print["large model: mean=", NumberForm[Mean[dLarge], {4,3}],
      "  min=", NumberForm[Min[dLarge], {4,3}],
      "  max=", NumberForm[Max[dLarge], {4,3}]];
Print["small model: mean=", NumberForm[Mean[dSmall], {4,3}],
      "  min=", NumberForm[Min[dSmall], {4,3}],
      "  max=", NumberForm[Max[dSmall], {4,3}]];

(* Pearson + Spearman correlation *)
pearson  = Correlation[dLarge, dSmall];
spearman = SpearmanRho[dLarge, dSmall];
Print["Pearson  r(large, small) = ", NumberForm[pearson, {4,3}]];
Print["Spearman \[Rho](large, small) = ", NumberForm[spearman, {4,3}]];

(* Rank disagreement: per-verse rank in each model, then absolute rank diff. *)
ranksLarge = Ordering[Ordering[-dLarge]];  (* rank 1 = most divergent *)
ranksSmall = Ordering[Ordering[-dSmall]];
rankDiff = Abs[ranksLarge - ranksSmall];

(* The top 5 disagreement verses. *)
indexed = Transpose[{Range[Length[romans]], rankDiff, ranksLarge, ranksSmall}];
topDisagree = Take[ReverseSortBy[indexed, #[[2]] &], 5];

Print["\n--- top-5 verses where the two models disagree most on rank ---"];
disagreement = Map[Function[row,
   Module[{i = row[[1]], delta = row[[2]], rL = row[[3]], rS = row[[4]]},
     Print["  ", romans[[i]]["ref"],
           "  rank_large=", rL, "  rank_small=", rS,
           "  |\[CapitalDelta]|=", delta,
           "  d_large=", NumberForm[dLarge[[i]], {4,3}],
           "  d_small=", NumberForm[dSmall[[i]], {4,3}]];
     <|"ref" -> romans[[i]]["ref"],
       "rank_large" -> rL, "rank_small" -> rS, "rank_delta" -> delta,
       "d_large" -> dLarge[[i]], "d_small" -> dSmall[[i]]|>]],
   topDisagree];

(* --- output JSON --- *)
out = <|
   "section"  -> "romans-8",
   "n_verses" -> Length[romans],
   "model_a"  -> "text-embedding-3-large (3072-d)",
   "model_b"  -> "text-embedding-3-small (1536-d)",
   "pearson"  -> pearson,
   "spearman" -> spearman,
   "per_verse" -> MapThread[
      <|"ref" -> #1["ref"], "d_large" -> #2, "d_small" -> #3|> &,
      {romans, dLarge, dSmall}],
   "top_disagreement" -> disagreement|>;
Export[FileNameJoin[{dataDir, "embedding_comparison.json"}], out, "JSON"];
Print["\nwrote data/embedding_comparison.json"];

(* --- scatter figure --- *)
points = MapThread[Tooltip[{#1, #2}, #3["ref"]] &,
   {dLarge, dSmall, romans}];

scatter = Show[
   ListPlot[points,
      PlotStyle -> Directive[PointSize[0.012],
         RGBColor[0.20, 0.40, 0.65, 0.75]],
      Frame -> True, GridLines -> Automatic,
      FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> "Helvetica",
                              FontSize -> 13],
      FrameLabel -> {
         Style["cosine d \[Dash] text-embedding-3-large (3072-d)",
            FontFamily -> "Helvetica", FontSize -> 14],
         Style["cosine d \[Dash] text-embedding-3-small (1536-d)",
            FontFamily -> "Helvetica", FontSize -> 14]},
      PlotLabel -> Style[
         "Romans 8 \[Dash] same KJV/modern pair, two embedding models",
         Bold, 15, FontFamily -> "Helvetica"],
      PlotRange -> {{0, 0.3}, {0, 0.5}},
      AspectRatio -> 0.75, ImageSize -> 1100],
   (* y = x reference line *)
   Plot[x, {x, 0, 0.3}, PlotStyle -> Directive[GrayLevel[0.55], Dashed,
       Thickness[0.0025]]],
   Epilog -> {
      GrayLevel[0.3], FontFamily -> "Helvetica", FontSize -> 13,
      Text[StringJoin["Spearman \[Rho] = ",
            ToString @ NumberForm[spearman, {4,3}]],
         Scaled[{0.78, 0.10}]],
      Text[StringJoin["Pearson  r = ",
            ToString @ NumberForm[pearson, {4,3}]],
         Scaled[{0.78, 0.05}]]}];

Export[FileNameJoin[{imgDir, "embedding_comparison_scatter.png"}], scatter];
Print["wrote docs/images/embedding_comparison_scatter.png"];
