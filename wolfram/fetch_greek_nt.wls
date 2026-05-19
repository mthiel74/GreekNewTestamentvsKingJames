#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Fetch the SBLGNT (Greek NT) from the `morphgnt/sblgnt` repo and
    reconstruct verse-level running Greek text at data/greek_nt.json.

    The morphgnt files are whitespace-separated, one word per line:

        010101  N-  ----NSF-  Βίβλος  Βίβλος  βίβλος  βίβλος
        ^ref    ^POS ^parsing  ^text   ^word   ^norm    ^lemma

    We keep column 4 (text, with punctuation) and concatenate
    within a verse.

    Source: Michael W. Holmes (ed.), *The Greek New Testament:
    SBL Edition* (SBL & Logos, 2010). Text under CC BY 4.0 via
    <https://github.com/morphgnt/sblgnt>. Morphology by J. K.
    Tauber, CC BY-SA 3.0.

    Run:
        wolframscript -file wolfram/fetch_greek_nt.wls
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "fetcher_common.wl"}];

(* {OSIS file prefix, full English book name, short abbreviation}.
   The OSIS prefix matches the morphgnt filename, e.g. 61-Mt-...
   The abbreviation matches the KJV fetcher so refs align. *)
ntBooks = {
   {61, "Matthew",         "Mt"},
   {62, "Mark",            "Mk"},
   {63, "Luke",            "Lk"},
   {64, "John",            "Jn"},
   {65, "Acts",            "Ac"},
   {66, "Romans",          "Ro"},
   {67, "1 Corinthians",   "1Co"},
   {68, "2 Corinthians",   "2Co"},
   {69, "Galatians",       "Ga"},
   {70, "Ephesians",       "Eph"},
   {71, "Philippians",     "Php"},
   {72, "Colossians",      "Col"},
   {73, "1 Thessalonians", "1Th"},
   {74, "2 Thessalonians", "2Th"},
   {75, "1 Timothy",       "1Ti"},
   {76, "2 Timothy",       "2Ti"},
   {77, "Titus",           "Tit"},
   {78, "Philemon",        "Phm"},
   {79, "Hebrews",         "Heb"},
   {80, "James",           "Jas"},
   {81, "1 Peter",         "1Pe"},
   {82, "2 Peter",         "2Pe"},
   {83, "1 John",          "1Jn"},
   {84, "2 John",          "2Jn"},
   {85, "3 John",          "3Jn"},
   {86, "Jude",            "Jud"},
   {87, "Revelation",      "Re"}
};

(* The morphgnt repo names files using its own abbreviation, which
   differs subtly from the KJV one (no "1Co", it's "1Co" too —
   but Phil is "Php"). We hardcode the morphgnt names. *)
morphgntAbbr = {
   "Mt","Mk","Lk","Jn","Ac","Ro","1Co","2Co","Ga","Eph","Php","Col",
   "1Th","2Th","1Ti","2Ti","Tit","Phm","Heb","Jas","1Pe","2Pe",
   "1Jn","2Jn","3Jn","Jud","Re"};

baseURL[osisN_, abbr_] := "https://raw.githubusercontent.com/morphgnt/sblgnt/master/" <>
   IntegerString[osisN] <> "-" <> abbr <> "-morphgnt.txt";

(* Parse a morphgnt text file: return list of {ref, text-token}. *)
parseMorphgnt[text_String] := Module[{lines, parts},
   lines = StringSplit[text, "\n"];
   lines = Select[lines, StringLength[#] > 0 &];
   Map[
     (parts = StringSplit[#, Whitespace];
      If[Length[parts] >= 4, {parts[[1]], parts[[4]]}, Nothing]) &,
     lines]
];

TPFetch`Banner["Fetching SBLGNT (morphgnt/sblgnt)"];

verses = {};

Do[
   Module[{osisN = ntBooks[[i, 1]], book = ntBooks[[i, 2]],
           abbr = ntBooks[[i, 3]], mAbbr = morphgntAbbr[[i]],
           dest, res, body, tokens, grouped, bookVerses},
     dest = FileNameJoin[{TPFetch`RawDir[], "sblgnt",
        IntegerString[osisN] <> "-" <> mAbbr <> "-morphgnt.txt"}];
     res = TPFetch`FetchText[baseURL[osisN, mAbbr], dest, 24*7];
     If[!res["ok"],
       Print["  FAIL ", book, ": ", res["error"]]; Continue[]];
     body = Import[dest, "Text"];
     tokens = parseMorphgnt[body];
     grouped = GroupBy[tokens, First -> Last, StringRiffle[#, " "] &];
     bookVerses = KeyValueMap[
        Function[{ref, txt},
          Module[{ch, v},
            ch = ToExpression @ StringTake[ref, {3, 4}];
            v  = ToExpression @ StringTake[ref, {5, 6}];
            <|"ref"  -> abbr <> " " <> ToString[ch] <> ":" <> ToString[v],
              "book" -> book, "abbr" -> abbr,
              "ch"   -> ch, "v" -> v, "text" -> txt|>]],
        grouped];
     verses = Join[verses, bookVerses];
     Print["  ", If[res["cached"], "cached", "fetched"], "  ",
           StringPadRight[book, 18], "  ",
           Length[bookVerses], " verses, ",
           Length[tokens], " tokens"];
   ],
   {i, Length[ntBooks]}];

(* The morphgnt files iterate book → ch → v in order, but
   KeyValueMap on GroupBy preserves first-seen key order, so the
   per-book list is already in canonical order. Concatenating
   per-book is therefore canonical NT order too. *)

outPath = FileNameJoin[{TPFetch`DataDir[], "greek_nt.json"}];
TPFetch`WriteJSON[outPath, verses];

Print["\nWrote ", Length[verses], " verses to ", outPath,
      " (", FileByteCount[outPath], " bytes)"];
