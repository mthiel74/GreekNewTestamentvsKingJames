#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Fetch Romans 8 from six public-domain English New Testament
    translations via bible.helloao.org. Used by the translation-drift
    phylogeny in \[Section] 7 and the animated drift chart in \[Section] 1.

    Output: data/historical_bibles_romans8.json — one Association per
    translation, each with a verses sub-association keyed "1", "2", ...

    Translations (chronological):
       Tyndale       1525-34  (eng_tnt)
       Geneva        1599     (eng_gnv)
       KJV           1611/1769 (eng_kjv)
       ASV           1901     (eng_asv)
       BBE           1949     (eng_bbe)
       WEB           2000     (ENGWEBP)

    All texts via the Free Use Bible API (bible.helloao.org) which
    redistributes them under their original public-domain status.

    Run:
        wolframscript -file wolfram/fetch_historical_bibles.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "fetcher_common.wl"}];

translations = {
   <|"id" -> "eng_tnt",  "short" -> "Tyndale", "year" -> 1534|>,
   <|"id" -> "eng_gnv",  "short" -> "Geneva",  "year" -> 1599|>,
   <|"id" -> "eng_kjv",  "short" -> "KJV",     "year" -> 1611|>,
   <|"id" -> "eng_asv",  "short" -> "ASV",     "year" -> 1901|>,
   <|"id" -> "eng_bbe",  "short" -> "BBE",     "year" -> 1949|>,
   <|"id" -> "ENGWEBP",  "short" -> "WEB",     "year" -> 2000|>
};

(* WL's RawJSON importer chokes on some Unicode codepoints in a few of
   the helloao payloads. We fall back to a regex pull \[Dash] the verse
   shape is very stable: {"type":"verse","number":N,"content":["text"]}. *)
unescapeJSON[s_String] :=
   StringReplace[s, {
      "\\n" -> "\n", "\\t" -> "\t",
      "\\\"" -> "\"", "\\\\" -> "\\",
      "\\/" -> "/"}];

extractVersesByRegex[raw_String] := Module[{matches},
   matches = StringCases[raw,
      RegularExpression[
         "\\{\"type\"\\s*:\\s*\"verse\"\\s*,\\s*\"number\"\\s*:\\s*(\\d+)\\s*,\\s*\"content\"\\s*:\\s*\\[\"((?:\\\\.|[^\"\\\\])*)\"\\]"]
      :> ("$1" -> unescapeJSON["$2"])];
   Association[matches]
];

fetchOne[t_Association] := Module[
   {url, dest, res, raw, body, content, verses},
   url  = "https://bible.helloao.org/api/" <> t["id"] <> "/ROM/8.json";
   dest = FileNameJoin[{TPFetch`RawDir[],
            "historical_" <> t["id"] <> "_rom8.json"}];
   res = TPFetch`FetchText[url, dest, 24*30];
   If[!res["ok"], Print["  FAIL ", t["short"], ": ", res["error"]];
                  Return[$Failed]];
   raw  = Import[dest, "Text"];
   body = Quiet @ ImportString[raw, "RawJSON"];
   verses = If[AssociationQ[body] && KeyExistsQ[body, "chapter"],
      content = body["chapter"]["content"];
      Association @ Cases[content,
         v_Association /; v["type"] === "verse" :>
            (ToString[v["number"]] -> StringRiffle[Flatten[{v["content"]}], " "])],
      (* RawJSON failed: regex fallback. *)
      extractVersesByRegex[raw]
   ];
   <|"id" -> t["id"], "short" -> t["short"], "year" -> t["year"],
     "verses" -> verses|>
];

TPFetch`Banner["Fetching historical PD English NT translations \[Dash] Romans 8"];

results = Map[
   Function[t,
     Print["  ", StringPadRight[t["short"], 10], " (", t["year"], ") \[Dash] ",
           t["id"]];
     With[{r = fetchOne[t]},
        If[r === $Failed, Nothing,
           Print["    ", Length[r["verses"]], " verses fetched"];
           r]]],
   translations];

out = FileNameJoin[{TPFetch`DataDir[], "historical_bibles_romans8.json"}];
TPFetch`WriteJSON[out, results];
Print["\nwrote ", out, " (",
      Length[results], " translations, total ",
      Total[Length[#["verses"]] & /@ results], " verses)"];
