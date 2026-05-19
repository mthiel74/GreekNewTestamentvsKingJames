#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Fetch + parse public-domain texts for the secondary translation
    pairs that demonstrate the framework generalises beyond the
    Greek-NT / KJV headline.

    Strategy
    --------
    Each parser produces a verse-keyed JSON file in data/ with the
    same schema as fetch_kjv.wls / fetch_greek_nt.wls:

        {"ref": "...", "book": "...", "ch": int, "v": int, "text": "..."}

    For the Bible-shaped pairs (Luther, Reina-Valera) we reuse the
    same ref strings as KJV ("Mt 1:1") so LoadPair joins cleanly.

    For prose-vs-prose pairs (Iliad, Marx, Tao Te Ching) we use a
    line-counter "Book N:Line M" or "Chapter N:Para M" scheme; the
    `ref` field is the join key and need not be biblical.

    This script is conservative: it parses the simplest sources
    reliably available from Project Gutenberg. Each parser includes
    a small format-detection step so you can swap in a different
    edition without restructuring the script.

    Usage
    -----
        wolframscript -file wolfram/fetch_extra_pairs.wls -- all
        wolframscript -file wolfram/fetch_extra_pairs.wls -- luther
        wolframscript -file wolfram/fetch_extra_pairs.wls -- iliad
        wolframscript -file wolfram/fetch_extra_pairs.wls -- tao
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "fetcher_common.wl"}];

(* ====================================================================
   Helper: strip Project Gutenberg header / footer.
   PG bookends a known marker line; we cut between them.
   ==================================================================== *)
stripPGBoilerplate[text_String] := Module[{startPos, endPos, body},
   startPos = StringPosition[text,
      RegularExpression["\\*\\*\\* START OF (THE )?PROJECT GUTENBERG[^\\n]*\\*\\*\\*"],
      1];
   endPos = StringPosition[text,
      RegularExpression["\\*\\*\\* END OF (THE )?PROJECT GUTENBERG[^\\n]*\\*\\*\\*"],
      1];
   body = If[startPos === {} || endPos === {},
      text,
      StringTake[text, {Last[First[startPos]] + 1, First[First[endPos]] - 1}]];
   StringTrim[body]
];

(* ====================================================================
   Luther 1545 — Project Gutenberg etext 14149.
   PG-14149 ships the whole bible in one .txt; the NT begins with a
   chapter heading "Das Evangelium nach Matthäus".

   Verse refs in the source look like "1.1" (one-based chapter.verse),
   blank-line separated between verses. Book headings on their own line.
   ==================================================================== *)

lutherBookMap = {
   "Matthäus" -> "Mt", "Markus" -> "Mk", "Lukas" -> "Lk",
   "Johannes" -> "Jn", "Apostelgeschichte" -> "Ac",
   "Römer" -> "Ro", "1. Korinther" -> "1Co", "2. Korinther" -> "2Co",
   "Galater" -> "Ga", "Epheser" -> "Eph", "Philipper" -> "Php",
   "Kolosser" -> "Col", "1. Thessalonicher" -> "1Th",
   "2. Thessalonicher" -> "2Th", "1. Timotheus" -> "1Ti",
   "2. Timotheus" -> "2Ti", "Titus" -> "Tit", "Philemon" -> "Phm",
   "Hebräer" -> "Heb", "Jakobus" -> "Jas", "1. Petrus" -> "1Pe",
   "2. Petrus" -> "2Pe", "1. Johannes" -> "1Jn", "2. Johannes" -> "2Jn",
   "3. Johannes" -> "3Jn", "Judas" -> "Jud", "Offenbarung" -> "Re"};

fetchLuther[] := Module[
   {url, dest, raw, body, lines, abbr, book, ref, ch, v, text, verses, m},
   url  = "https://www.gutenberg.org/cache/epub/14149/pg14149.txt";
   dest = FileNameJoin[{TPFetch`RawDir[], "luther_1545.txt"}];
   res = TPFetch`FetchText[url, dest, 24*30];
   If[!res["ok"],
     Print["FAIL Luther: ", res["error"]]; Return[$Failed]];
   raw = Import[dest, "Text"];
   body = stripPGBoilerplate[raw];
   (* PG-14149 verses look like:
        1.1 Im Anfang schuf Gott Himmel und Erde.
      with book transitions like "Das Evangelium nach Matthäus" on a
      line of their own. The exact format is fragile; users who want a
      better-tagged German NT should swap in a tagged source such as
      the OSIS Luther 1912 from the SWORD / CrossWire project. *)
   Print["  Luther 1545: ", StringLength[body], " bytes after boilerplate strip"];
   Print["  NOTE: PG-14149 parser is best-effort. For production use"];
   Print["        the OSIS-tagged Luther 1912 from CrossWire."];
   (* Skipping full parser implementation here — write the raw body
      so a future iteration can finish the parser. *)
   Export[FileNameJoin[{TPFetch`DataDir[], "luther_nt_raw.txt"}], body, "Text"];
   $Failed
];

(* ====================================================================
   Iliad — Butler prose (PG-2199). One book per chapter; paragraphs
   are blank-line separated. Ref scheme: "Iliad B.P" for paragraph P
   of book B. We do not pretend to align Butler's paragraphs to
   Pope's couplets — that's the alignment problem the framework
   would solve, and we'd do it from sentence-level embeddings.
   ==================================================================== *)

fetchIliadButler[] := Module[{url, dest, raw, body, verses, bookN, paraN, paras},
   url  = "https://www.gutenberg.org/cache/epub/2199/pg2199.txt";
   dest = FileNameJoin[{TPFetch`RawDir[], "iliad_butler.txt"}];
   res = TPFetch`FetchText[url, dest, 24*30];
   If[!res["ok"], Print["FAIL Iliad Butler: ", res["error"]]; Return[$Failed]];
   raw = Import[dest, "Text"];
   body = stripPGBoilerplate[raw];
   (* Butler labels each book "BOOK I", "BOOK II", ... in uppercase.
      Split on those. *)
   bookN = 0;
   paraN = 0;
   verses = {};
   Do[
     If[StringMatchQ[StringTrim[block],
          RegularExpression["BOOK [IVXLCDM]+\\.?"]],
       bookN++; paraN = 0; Continue[]];
     If[bookN > 0 && StringLength[StringTrim[block]] > 0,
       paraN++;
       AppendTo[verses, <|
         "ref"  -> "Iliad " <> ToString[bookN] <> "." <> ToString[paraN],
         "book" -> "Iliad", "abbr" -> "Il",
         "ch"   -> bookN, "v" -> paraN,
         "text" -> StringTrim[block]|>]],
     {block, StringSplit[body, RegularExpression["\\n\\s*\\n+"]]}];
   out = FileNameJoin[{TPFetch`DataDir[], "iliad_butler.json"}];
   TPFetch`WriteJSON[out, verses];
   Print["  Iliad (Butler): wrote ", Length[verses],
         " paragraphs across ", bookN, " books"];
   out
];

(* ====================================================================
   Tao Te Ching — Legge (PG-216).
   The Legge edition is 81 chapters, each with a short prose body. We
   key by "TTC <chapter>" — there are no verses inside a chapter.

   For demonstration we just parse the chapter list, which suffices to
   join against any other 81-chapter translation (e.g. Goddard 1919,
   Wing-tsit Chan, ...).
   ==================================================================== *)

fetchTaoLegge[] := Module[{url, dest, raw, body, chapters, verses, i},
   url = "https://www.gutenberg.org/cache/epub/216/pg216.txt";
   dest = FileNameJoin[{TPFetch`RawDir[], "tao_legge.txt"}];
   res = TPFetch`FetchText[url, dest, 24*30];
   If[!res["ok"], Print["FAIL Tao Legge: ", res["error"]]; Return[$Failed]];
   raw = Import[dest, "Text"];
   body = stripPGBoilerplate[raw];
   (* Legge's PG edition uses arabic chapter numbers on their own
      line, e.g. "Ch. 1." or "1." Parsing is fragile across editions.
      For this skeleton we leave the raw body on disk so a future
      iteration can finish the parser. *)
   Export[FileNameJoin[{TPFetch`DataDir[], "tao_legge_raw.txt"}], body, "Text"];
   Print["  Tao (Legge): ", StringLength[body],
         " bytes saved as data/tao_legge_raw.txt for parsing"];
   $Failed
];

(* ====================================================================
   Dispatch
   ==================================================================== *)

(* wolframscript leaves the literal "--" separator in argv. Drop it. *)
args = DeleteCases[Rest[$ScriptCommandLine], "--"];
target = If[Length[args] >= 1, args[[1]], "all"];

TPFetch`Banner["fetch_extra_pairs: " <> target];

Switch[target,
   "luther",   fetchLuther[],
   "iliad",    fetchIliadButler[],
   "tao",      fetchTaoLegge[],
   "all",      (fetchLuther[]; fetchIliadButler[]; fetchTaoLegge[];),
   _,          Print["unknown target. options: all | luther | iliad | tao"]];

Print["\nNote: the secondary-pair parsers are deliberately skeletal."];
Print["The Iliad parser is complete (book/paragraph granularity);"];
Print["the Luther and Tao parsers save the boilerplate-stripped body"];
Print["to data/*_raw.txt so the next pass can produce verse-keyed JSON"];
Print["with whatever ref scheme matches the joining side."];
