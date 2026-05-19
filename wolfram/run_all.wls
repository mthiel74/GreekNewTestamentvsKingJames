#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  One-shot driver: fetch → embed → analyse for the headline
    Greek-NT / KJV pair. Other pairs are run by re-invoking with
    a pair name (analogous to embed.wls / analyze.wls).

    Usage:
        wolframscript -file wolfram/run_all.wls
            full pipeline on greek-nt-kjv (default)

    Env vars consumed:
        COHERE_API_KEY | OPENAI_API_KEY | VOYAGE_API_KEY  (embed)
        ANTHROPIC_API_KEY | OPENAI_API_KEY                (LLM pass)
        TP_EMBED_PROVIDER  override embedder, default cohere
        TP_TOPN            number of divergent verses for LLM pass

    No keys? The fetchers still run; embed.wls and analyze.wls exit
    cleanly with a clear message.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];

run[script_, args___] := Module[{cmd, ret},
   cmd = StringRiffle[{
      "wolframscript", "-file",
      FileNameJoin[{repoRoot, "wolfram", script}],
      "--", args}, " "];
   Print["\n>>> ", cmd];
   ret = RunProcess[{
      "wolframscript", "-file",
      FileNameJoin[{repoRoot, "wolfram", script}],
      "--", args}];
   Print[ret["StandardOutput"]];
   If[ret["ExitCode"] =!= 0,
     Print["!! ", script, " exited ", ret["ExitCode"]];
     If[StringLength[ret["StandardError"]] > 0,
       Print[ret["StandardError"]]];
   ];
   ret["ExitCode"]
];

topN = Environment["TP_TOPN"];
If[topN === None || topN === "", topN = "50"];

Print["=== Greek NT vs KJV — full pipeline ==="];

If[run["fetch_kjv.wls"]       =!= 0, Exit[1]];
If[run["fetch_greek_nt.wls"]  =!= 0, Exit[1]];
If[run["embed.wls",    "greek-nt-kjv"] =!= 0, Exit[1]];
If[run["analyze.wls",  "greek-nt-kjv", topN] =!= 0, Exit[1]];

Print["\n=== done ==="];
Print["  ranked table:  data/greek-nt-kjv_ranked.json"];
Print["  LLM top-N:     data/greek-nt-kjv_top", topN, "_explained.json"];
Print["  figures:       docs/images/greek-nt-kjv_*.png"];
