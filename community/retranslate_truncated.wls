#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Detect translations in section_<key>_translated.json whose
    output looks truncated (no terminal punctuation; or much shorter
    than the KJV side) and re-translate them with the fresh, higher
    max_completion_tokens budget. Then recompute the KJV/modern
    embeddings and cosine distances for the section.

    Usage:
        wolframscript -file wolfram/retranslate_truncated.wls -- romans-8
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

args = DeleteCases[Rest[$ScriptCommandLine], "--"];
section = If[Length[args] >= 1, args[[1]], "romans-8"];

inFile = FileNameJoin[{repoRoot, "data",
   "section_" <> section <> "_translated.json"}];
If[!FileExistsQ[inFile], Print["missing: ", inFile]; Exit[2]];

data = Import[inFile, "RawJSON"];
Print["loaded ", Length[data], " rows from ", inFile];

(* Heuristic: missing terminal punctuation, or modern < 60% of KJV length. *)
isTruncated[row_] :=
   ! StringMatchQ[StringTake[row["modern"], -1], "." | "!" | "?" | ":"] &&
   StringLength[row["modern"]] < StringLength[row["kjv"]];

flagged = Select[data, isTruncated];
Print["flagged ", Length[flagged], " possibly-truncated translations"];
If[flagged === {}, Print["nothing to do"]; Exit[0]];

(* Retranslate. *)
fixes = AssociationMap[
   Function[r,
     Print["  retranslating ", r["ref"], " ..."];
     With[{t = LLMTranslate[r["greek"], r["ref"]]},
       If[t === $Failed, r["modern"], t]]],
   flagged];

(* fixes is keyed by row Association \[Dash] easier to work by ref. *)
fixesByRef = AssociationThread[
   #["ref"] & /@ flagged -> Values[fixes]];

newData = Map[
   If[KeyExistsQ[fixesByRef, #["ref"]],
      Append[#, "modern" -> fixesByRef[#["ref"]]],
      #] &,
   data];

(* Recompute embeddings + distances for the whole section
   (cache hits make the un-changed verses free). *)
Print["recomputing embeddings + distances ..."];
embK = EmbedTexts[#["kjv"]    & /@ newData, EmbeddingProvider -> "openai"];
embM = EmbedTexts[#["modern"] & /@ newData, EmbeddingProvider -> "openai"];
dists = MapThread[CosineDistance, {embK, embM}];

reranked = SortBy[
   MapThread[Append[#1, "distance" -> #2] &, {newData, dists}],
   -#["distance"] &];

Export[inFile, reranked, "JSON"];
Print["wrote updated ", inFile];

(* Re-draw the distance figure with the corrected ordering. *)
imgDir = FileNameJoin[{repoRoot, "docs", "images"}];
verseN[row_] := ToExpression @ Last @ StringSplit[row["ref"], ":"];
plotRows = SortBy[reranked, verseN];
plot = ListPlot[
   MapThread[Tooltip[{verseN[#1], #1["distance"]}, #1["ref"]] &,
     {plotRows, plotRows}],
   Filling -> Axis, FillingStyle -> RGBColor[0.2, 0.4, 0.7, 0.4],
   PlotStyle -> Directive[PointSize[0.012], RGBColor[0.2, 0.4, 0.7]],
   Frame -> True, GridLines -> Automatic,
   FrameLabel -> {"verse", "cosine distance (KJV vs modern, both English)"},
   PlotLabel -> Style[ToUpperCase @ StringReplace[section, "-" -> " "] <>
                  " \[Dash] per-verse divergence", Bold, 14],
   ImageSize -> 800];
Export[FileNameJoin[{imgDir, "section_" <> section <> "_distance.png"}], plot];
Print["redrew distance figure"];
