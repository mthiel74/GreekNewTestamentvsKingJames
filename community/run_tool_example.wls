#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Produce a worked example for the translator-tool section of the
    notebook. Saves to data/translator_tool_example.json so the build
    script can show a frozen instance of the live tool's output in
    the PDF.

    Case: 1 Corinthians 13:1 \[Dash] the "tongues of angels" verse,
    three candidate English renderings (KJV, NIV, a deliberately
    paraphrased rendering for contrast).

    Run:
        wolframscript -file wolfram/run_tool_example.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Get @ FileNameJoin[{repoRoot, "wolfram", "translator_tool.wl"}];
Needs["TranslatorTool`"];

(* Use the Greek of 1 Cor 13:1 from the canonical SBLGNT JSON, so
   we don't have inline Greek in this source file. *)
gnt = Import[FileNameJoin[{repoRoot, "data", "greek_nt.json"}], "RawJSON"];
source = SelectFirst[gnt, #["ref"] == "1Co 13:1" &]["text"];

candidates = {
   (* 1. KJV 1769 *)
   "Though I speak with the tongues of men and of angels, and have not charity, I am become as sounding brass, or a tinkling cymbal.",
   (* 2. NIV-style modern *)
   "If I speak in the tongues of men or of angels, but do not have love, I am only a resounding gong or a clanging cymbal.",
   (* 3. A deliberately paraphrasing rendering, for contrast *)
   "If my words have all the beauty of every human dialect and every angelic song, yet I am loveless, I am no more than an empty echo."
};

Print["running translator-tool audit on 1 Cor 13:1 ..."];
res = TranslatorTool`RunAudit[source, "Koine Greek", candidates];

out = FileNameJoin[{repoRoot, "data", "translator_tool_example.json"}];
Export[out, res, "JSON"];
Print["wrote ", out];

Print["\n--- summary ---"];
Print["source: ", source];
Print["baseline: ", res["baseline"]];
Do[
  Print["\nCandidate ", c["index"], "  d=",
        NumberForm[c["distance"], {4,3}]];
  Print["  text: ", c["candidate"]];
  n = res["commentary"][[i]];
  Print["  distinctive_choice: ", n["distinctive_choice"]];
  Print["  key_lemma:          ", n["key_lemma"]];
  Print["  suggested_alt:      ", n["suggested_alt"]];
  Print["  explanation:        ", StringTake[n["explanation"], UpTo[120]], "..."],
  {i, Length[res["candidates"]]}, {c, {res["candidates"][[i]]}}];
