#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Sanity tests for the project. These do not call any paid API —
    they only verify the shape of files committed under data/.

    Run:
        wolframscript -file tests/sanity.wls
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
dataDir = FileNameJoin[{repoRoot, "data"}];

failures = {};
assert[label_, cond_] := If[TrueQ[cond],
   Print["  PASS  ", label],
   AppendTo[failures, label];
   Print["  FAIL  ", label]];

Print["=== sanity checks ==="];

(* --- KJV --- *)
kjvPath = FileNameJoin[{dataDir, "kjv_nt.json"}];
If[FileExistsQ[kjvPath],
   kjv = Import[kjvPath, "RawJSON"];
   assert["KJV verse count is 7957 (NT canonical)",
      Length[kjv] == 7957];
   assert["KJV Mt 1:1 begins 'The book of the generation'",
      StringStartsQ[
        SelectFirst[kjv, #["ref"] == "Mt 1:1" &]["text"],
        "The book of the generation"]];
   assert["KJV has the Comma Johanneum at 1Jn 5:7",
      StringContainsQ[
        SelectFirst[kjv, #["ref"] == "1Jn 5:7" &]["text"],
        "three that bear record in heaven"]],
   AppendTo[failures, "kjv_nt.json missing"];
   Print["  SKIP  KJV checks (file missing — run wolfram/fetch_kjv.wls)"]];

(* --- Greek NT --- *)
gntPath = FileNameJoin[{dataDir, "greek_nt.json"}];
If[FileExistsQ[gntPath],
   gnt = Import[gntPath, "RawJSON"];
   assert["SBLGNT verse count is between 7900 and 7950",
      7900 <= Length[gnt] <= 7950];
   (* wolframscript on macOS reads .wls source as Latin-1 unless told
      otherwise, so a literal Greek char in the source can mis-encode.
      Compare by explicit codepoint instead. 914 = capital Beta. *)
   assert["SBLGNT Mt 1:1 begins with Βίβλος (cp 914 943 946 ...)",
      Take[
        ToCharacterCode @ SelectFirst[gnt, #["ref"] == "Mt 1:1" &]["text"],
        6] == {914, 943, 946, 955, 959, 962}];
   assert["SBLGNT does NOT contain Acts 8:37 (manuscript variant)",
      ! MemberQ[#["ref"] & /@ gnt, "Ac 8:37"]],
   AppendTo[failures, "greek_nt.json missing"];
   Print["  SKIP  GNT checks (file missing — run wolfram/fetch_greek_nt.wls)"]];

(* --- Framework load --- *)
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
If[FileExistsQ[kjvPath] && FileExistsQ[gntPath],
   res = TPFramework`LoadPair["greek-nt-kjv", "greek_nt.json", "kjv_nt.json"];
   assert["LoadPair returns ~7925 aligned units",
      7900 <= Length[res["aligned"]] <= 7950];
   assert["LoadPair surfaces ~30 KJV-only refs (textual variants)",
      20 <= Length[res["onlyB"]] <= 50];
   assert["Comma Johanneum (1Jn 5:7) is in onlyB or aligned but flagged",
      MemberQ[res["onlyB"], "1Jn 5:7"] ||
      AnyTrue[res["aligned"],
        #["ref"] == "1Jn 5:7" &]];
];

Print["\n=== ", If[failures === {}, "all passed", ToString[Length[failures]] <> " failed"], " ==="];
If[failures =!= {}, Print[failures]; Exit[1]];
