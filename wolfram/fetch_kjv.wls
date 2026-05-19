#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Fetch the 27 NT books of the King James Version (1769 Cambridge)
    from the `aruljohn/Bible-kjv` GitHub repo as per-book JSON, and
    flatten them into a single verse-keyed JSON file at
    data/kjv_nt.json.

    Output schema (one verse per element):
        {
           "ref":   "Mt 1:1",            (* canonical short ref     *)
           "book":  "Matthew",           (* full book name          *)
           "abbr":  "Mt",
           "ch":    1, "v": 1,
           "text":  "The book of the generation ..."
        }

    Run:
        wolframscript -file wolfram/fetch_kjv.wls
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "fetcher_common.wl"}];

(* NT canon order + 2-letter abbreviation, matching the SBLGNT
   numbering (Matt = 01 within the NT). *)
ntBooks = {
   {"Matthew",         "Mt"},
   {"Mark",            "Mk"},
   {"Luke",            "Lk"},
   {"John",            "Jn"},
   {"Acts",            "Ac"},
   {"Romans",          "Ro"},
   {"1 Corinthians",   "1Co"},
   {"2 Corinthians",   "2Co"},
   {"Galatians",       "Ga"},
   {"Ephesians",       "Eph"},
   {"Philippians",     "Php"},
   {"Colossians",      "Col"},
   {"1 Thessalonians", "1Th"},
   {"2 Thessalonians", "2Th"},
   {"1 Timothy",       "1Ti"},
   {"2 Timothy",       "2Ti"},
   {"Titus",           "Tit"},
   {"Philemon",        "Phm"},
   {"Hebrews",         "Heb"},
   {"James",           "Jas"},
   {"1 Peter",         "1Pe"},
   {"2 Peter",         "2Pe"},
   {"1 John",          "1Jn"},
   {"2 John",          "2Jn"},
   {"3 John",          "3Jn"},
   {"Jude",            "Jud"},
   {"Revelation",      "Re"}
};

(* aruljohn/Bible-kjv collapses spaces in numbered books:
   "1 Corinthians" -> 1Corinthians.json. All non-numbered books
   are already single-word, so a blanket StringDelete is correct. *)
baseURL[book_] := "https://raw.githubusercontent.com/aruljohn/Bible-kjv/master/" <>
   StringDelete[book, " "] <> ".json";

TPFetch`Banner["Fetching KJV NT (aruljohn/Bible-kjv)"];

verses = {};

Do[
   Module[{book = ntBooks[[i, 1]], abbr = ntBooks[[i, 2]],
           dest, res, data, chs},
     dest = FileNameJoin[{TPFetch`RawDir[], "kjv", abbr <> ".json"}];
     res = TPFetch`FetchText[baseURL[book], dest, 24*7];
     If[!res["ok"],
       Print["  FAIL ", book, ": ", res["error"]];
       Continue[]];
     Print["  ", If[res["cached"], "cached", "fetched"], "  ",
           StringPadRight[book, 18], "  ", res["bytes"], " bytes"];
     data = TPFetch`ReadJSON[dest];
     chs = data["chapters"];
     Do[
       Module[{chNum = ToExpression @ ch["chapter"]},
         Do[
           AppendTo[verses, <|
             "ref"  -> abbr <> " " <> ToString[chNum] <> ":" <> v["verse"],
             "book" -> book, "abbr" -> abbr,
             "ch"   -> chNum,
             "v"    -> ToExpression @ v["verse"],
             "text" -> v["text"]
           |>],
           {v, ch["verses"]}]],
       {ch, chs}];
   ],
   {i, Length[ntBooks]}];

outPath = FileNameJoin[{TPFetch`DataDir[], "kjv_nt.json"}];
TPFetch`WriteJSON[outPath, verses];

Print["\nWrote ", Length[verses], " verses to ", outPath,
      " (", FileByteCount[outPath], " bytes)"];
