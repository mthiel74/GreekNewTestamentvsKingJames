(* ::Package:: *)

(* Shared HTTP + cache helpers for the translation-pair fetchers.
   Pattern lifted from ENSO-emergence/wolfram/fetcher_common.wl —
   same `URLRead[HTTPRequest[...]]` + status-code check + on-disk
   cache idiom.
*)

BeginPackage["TPFetch`"];

RepoRoot::usage = "RepoRoot[] returns the absolute path of the repo root.";
DataDir::usage  = "DataDir[] / RawDir[] absolute paths.";
RawDir::usage   = "DataDir[] / RawDir[] absolute paths.";
Banner::usage   = "Banner[msg] prints a section banner.";

FetchText::usage  = "FetchText[url, dest, maxAgeHours] downloads URL as text, with on-disk cache.";
FetchBytes::usage = "FetchBytes[url, dest, maxAgeHours] downloads URL as bytes, with on-disk cache.";

WriteJSON::usage  = "WriteJSON[path, expr] writes expr as pretty JSON to path.";
ReadJSON::usage   = "ReadJSON[path] reads JSON, returning Association-style data.";

Begin["`Private`"];

RepoRoot[] := ParentDirectory @ DirectoryName[$InputFileName];
DataDir[]  := FileNameJoin[{RepoRoot[], "data"}];
RawDir[]   := FileNameJoin[{DataDir[], "raw"}];

ensureDir[p_] := If[!DirectoryQ[p],
   CreateDirectory[p, CreateIntermediateDirectories -> True]];

ensureDir[DataDir[]]; ensureDir[RawDir[]];

Banner[msg_] := Print["\n=== ", msg, " ==="];

fileAgeHours[path_] := If[FileExistsQ[path],
   QuantityMagnitude @ DateDifference[
     FileDate[path, "Modification"], Now, "Hour"
   ], Infinity];

fetchWith[url_, dest_, mode_, maxAgeHours_] := Module[
   {age, resp, status, body},
   ensureDir[DirectoryName[dest]];
   age = fileAgeHours[dest];
   If[NumericQ[maxAgeHours] && age < maxAgeHours && FileExistsQ[dest],
     Return[<|
       "ok" -> True, "url" -> url, "savedTo" -> dest,
       "bytes" -> FileByteCount[dest], "cached" -> True
     |>]];
   resp = Quiet @ URLRead @ HTTPRequest[url];
   If[FailureQ[resp],
     Return[<|"ok" -> False, "url" -> url, "savedTo" -> dest,
       "bytes" -> 0, "cached" -> False, "error" -> ToString[resp]|>]];
   status = resp["StatusCode"];
   If[!IntegerQ[status] || status < 200 || status >= 300,
     Return[<|"ok" -> False, "url" -> url, "savedTo" -> dest,
       "bytes" -> 0, "cached" -> False,
       "error" -> "HTTP " <> ToString[status]|>]];
   body = Switch[mode,
     "Text",  resp["Body"],
     "Bytes", resp["BodyBytes"]];
   If[mode === "Text",
     Export[dest, body, "Text"],
     (* BinaryWrite leaves an open stream — close it explicitly *)
     With[{s = OpenWrite[dest, BinaryFormat -> True]},
       BinaryWrite[s, body]; Close[s]]];
   <|"ok" -> True, "url" -> url, "savedTo" -> dest,
     "bytes" -> FileByteCount[dest], "cached" -> False|>
];

FetchText[url_, dest_, maxAge_:24] := fetchWith[url, dest, "Text", maxAge];
FetchBytes[url_, dest_, maxAge_:24] := fetchWith[url, dest, "Bytes", maxAge];

WriteJSON[path_, expr_] := (
   ensureDir[DirectoryName[path]];
   Export[path, expr, "JSON"];
   path
);
ReadJSON[path_] := Import[path, "RawJSON"];

End[];
EndPackage[];
