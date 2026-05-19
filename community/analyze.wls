#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Rank aligned units by cosine distance, render the headline
    figures, and have an LLM hypothesise *why* each of the top-N
    divergent units diverged.

    Outputs:
        data/<pair>_ranked.json              full ranked table
        data/<pair>_topN_explained.json      top-N with LLM tags
        docs/images/<pair>_distance_hist.png
        docs/images/<pair>_distance_by_book.png

    Usage:
        wolframscript -file wolfram/analyze.wls -- <pair> <topN>
            pair defaults to "greek-nt-kjv"; topN defaults to 50.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];

args = DeleteCases[Rest[$ScriptCommandLine], "--"];
pair = If[Length[args] >= 1, args[[1]], "greek-nt-kjv"];
topN = If[Length[args] >= 2, ToExpression[args[[2]]], 50];

provider = Environment["TP_EMBED_PROVIDER"];
If[provider === None || provider === "", provider = "cohere"];

embedFile = FileNameJoin[{repoRoot, "data", "embeddings",
   pair <> "_" <> provider <> ".mx"}];

If[!FileExistsQ[embedFile],
   Print["Missing embedding file: ", embedFile];
   Print["Run: wolframscript -file wolfram/embed.wls -- ", pair];
   Exit[2]];

Get[embedFile]; (* defines embedPair *)
aligned = embedPair["aligned"];
embA = embedPair["embA"]; embB = embedPair["embB"];

Print["=== Analyzing pair: ", pair, " (", Length[aligned], " aligned units) ==="];

ranked = TPFramework`RankDivergence[aligned, embA, embB];

(* --- Save the full ranked table --- *)
rankedOut = FileNameJoin[{repoRoot, "data", pair <> "_ranked.json"}];
Export[rankedOut, ranked, "JSON"];
Print["  wrote ", rankedOut];

(* --- Headline figure: distance histogram --- *)
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];
If[!DirectoryQ[imgDir], CreateDirectory[imgDir, CreateIntermediateDirectories -> True]];

distances = #["distance"] & /@ ranked;
hist = Histogram[distances, 80,
   PlotLabel -> Style["Cosine distance: " <> pair, Bold, 14],
   FrameLabel -> {"cosine distance", "verses"},
   Frame -> True, GridLines -> Automatic, ImageSize -> 640];
Export[FileNameJoin[{imgDir, pair <> "_distance_hist.png"}], hist];
Print["  wrote distance histogram"];

(* --- Per-book mean / 95th-percentile distance --- *)
byBook = GroupBy[ranked, #["book"] &];
bookStats = KeyValueMap[
   {#1, Mean[#["distance"] & /@ #2], Quantile[#["distance"] & /@ #2, 0.95]} &,
   byBook];

bookPlot = BarChart[
   #[[2]] & /@ bookStats,
   ChartLabels -> Placed[#[[1]] & /@ bookStats, Below, Rotate[#, 0.5] &],
   PlotLabel -> Style["Mean cosine distance by book (" <> pair <> ")", Bold, 14],
   Frame -> True, FrameLabel -> {"", "mean distance"},
   ImageSize -> 900];
Export[FileNameJoin[{imgDir, pair <> "_distance_by_book.png"}], bookPlot];
Print["  wrote per-book bar chart"];

(* --- LLM hypothesis pass on the top-N --- *)
hasLLM = (Environment["ANTHROPIC_API_KEY"] =!= None &&
          Environment["ANTHROPIC_API_KEY"] =!= "") ||
         (Environment["OPENAI_API_KEY"] =!= None &&
          Environment["OPENAI_API_KEY"] =!= "");

If[!hasLLM,
   Print["  no LLM key set — skipping hypothesis pass."];
   Print["  set ANTHROPIC_API_KEY (or OPENAI_API_KEY) to run it."];
   Exit[0]];

llmProvider = If[Environment["ANTHROPIC_API_KEY"] =!= None &&
                 Environment["ANTHROPIC_API_KEY"] =!= "",
   "anthropic", "openai"];

Print["  LLM hypothesis pass via ", llmProvider, " on top ", topN, " rows"];

topRows = Take[ranked, UpTo[topN]];
explained = MapIndexed[
   (Print["    ", First[#2], "/", Length[topRows], "  ", #1["ref"],
          "  d=", NumberForm[#1["distance"], {4, 3}]];
    TPFramework`LLMExplain[#1,
       TPFramework`LLMProvider -> llmProvider]) &,
   topRows];
explained = Select[explained, # =!= $Failed &];

explainedOut = FileNameJoin[{repoRoot, "data",
   pair <> "_top" <> ToString[topN] <> "_explained.json"}];
Export[explainedOut, explained, "JSON"];
Print["  wrote ", explainedOut];
