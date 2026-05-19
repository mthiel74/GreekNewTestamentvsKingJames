#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  News-fact-checker case study: §10 of the notebook.

    Three multilingual public-domain texts:
       1. UDHR Article 1            (Spanish source, UN, 1948)
       2. Laudato Si' §1 opening    (Italian source, Vatican, 2015)
       3. UN Charter Article 1 ¶1   (French source, UN, 1945)

    For each, we run TranslatorTool`Audit on three hand-written
    candidate English renderings. The audit produces:
       - an independent gpt-5.4 baseline from the source language
       - cosine distance for each candidate against that baseline
       - per-candidate nuance commentary

    Output:
       data/news_fact_checker.json (full audit per case)

    Run:
        wolframscript -file wolfram/news_fact_checker.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Get @ FileNameJoin[{repoRoot, "wolfram", "translator_tool.wl"}];
Needs["TranslatorTool`"];

sources = Import[FileNameJoin[{repoRoot, "data",
   "news_fact_checker_sources.json"}], "RawJSON"];
Print["Loaded ", Length[sources], " case studies"];

results = Map[
   Function[c,
     Print["\n=== ", c["id"], "  (", c["title"], ") ==="];
     Print["  source language: ", c["sourceLang"]];
     Print["  candidates: ", Length[c["candidates"]]];
     With[{auditResult = TranslatorTool`RunAuditProgressive[
            c["source"], c["sourceLang"],
            (#["text"]) & /@ c["candidates"],
            Function[msg, Print["  ", msg]],
            TranslatorTool`Model -> "gpt-5.4"]},
        (* Pair each ranked candidate with its label *)
        Module[{labelByText},
           labelByText = AssociationThread[
              (#["text"]) & /@ c["candidates"],
              (#["label"]) & /@ c["candidates"]];
           Append[c, "audit" -> Append[auditResult,
              "candidate_labels" ->
                 (Lookup[labelByText, #["candidate"], "?"] & /@
                    auditResult["candidates"])]]]]],
   sources];

out = FileNameJoin[{repoRoot, "data", "news_fact_checker.json"}];
Export[out, results, "JSON"];
Print["\nwrote ", out];
