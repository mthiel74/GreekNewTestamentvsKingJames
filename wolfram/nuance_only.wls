#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Re-run only the GPT-5.5 nuance pass on a section that already has
    a *_translated.json file. Lets you iterate on the prompt without
    paying the translation cost each time.

    Usage:
        wolframscript -file wolfram/nuance_only.wls -- romans-8 10
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

args = DeleteCases[Rest[$ScriptCommandLine], "--"];
section = If[Length[args] >= 1, args[[1]], "romans-8"];
topN = If[Length[args] >= 2, ToExpression[args[[2]]], 10];

translatedFile = FileNameJoin[{repoRoot, "data",
   "section_" <> section <> "_translated.json"}];
If[!FileExistsQ[translatedFile],
   Print["No translated file at ", translatedFile];
   Print["Run wolfram/section_analysis.wls first."];
   Exit[2]];

ranked = Import[translatedFile, "RawJSON"];
Print["loaded ", Length[ranked], " translated verses from ", translatedFile];
Print["running GPT-5.5 nuance pass on top ", topN, " ..."];

nuanced = MapIndexed[
   (Print["  ", First[#2], "/", Min[topN, Length[ranked]], "  ",
          #1["ref"], "  d=", NumberForm[#1["distance"], {4, 3}]];
    LLMNuanceCompare[#1]) &,
   Take[ranked, UpTo[topN]]];
nuanced = Select[nuanced, AssociationQ];

out = FileNameJoin[{repoRoot, "data",
   "section_" <> section <> "_nuance.json"}];
Export[out, nuanced, "JSON"];
Print["wrote ", out, " (", Length[nuanced], " verses)"];
