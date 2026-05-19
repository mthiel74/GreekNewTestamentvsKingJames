#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Three-way generalisation: Tao Te Ching, chapters 1-2.
        - Chinese (Wang Bi recension; PG #7337)
        - Legge 1891 (PD, PG #216)
        - GPT-5.5 careful modern English rendering

    Demonstrates that the same translate-then-compare pipeline applies
    to a non-Bible, non-Western text pair. The interesting nuances
    here are abstract-noun choices (Tao, Te, ming, wu/yu) and the
    Victorian English register of Legge.

    Run:
        wolframscript -file wolfram/generalisation_tao.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

(* ====================================================================
   Source texts loaded from data/tao_passages.json (see the comment in
   generalisation_iliad.wls for why we keep them out of the .wls source).
   ==================================================================== *)
chapters = Import[FileNameJoin[{repoRoot, "data", "tao_passages.json"}],
   "RawJSON"];

(* GPT-5.5 system prompt: classical-Chinese translator focused on
   abstract-noun fidelity. *)
chineseTranslateSystem = "You are a translator of classical Chinese with a particular \
focus on the Daodejing. Render the user's Chinese passage into a careful, modern \
English translation calibrated for an academic audience (classics, comparative \
philosophy). Specifically: \n\
  - leave the term \[Dao] untranslated as 'the Dao' (do not say 'the Way' on \
its own without qualification); \n\
  - render \[De] as 'virtue' when ethical, 'inherent power' when ontological; \n\
  - preserve the contrastive parallelism 'wu/you' as 'without-form / with-form' \
or 'non-being / being' depending on context, never collapsing them; \n\
  - preserve sentence-level parallelism where the Chinese has it; \n\
  - do not import Christian-derived vocabulary (no 'Creator', no 'Mystery' for \
\[Xuan] \[Dash] prefer 'darkly profound' or 'deep darkness'); \n\
  - do not add subjects or modifiers the Chinese does not require. \n\n\
Reply with the English translation only \[Dash] no commentary, no quotation \
marks, no chapter number, no leading whitespace.";

callTranslate[chinese_String, ch_Integer] :=
   TPFramework`Private`openaiCall["Daodejing chapter " <> ToString[ch] <>
      ":\n\n" <> chinese, "gpt-5.5", 4000,
      TPFramework`OpenAIKey[], "high", chineseTranslateSystem, False];

(* Three-way nuance prompt: Legge vs modern English of a fresh
   modern translation, anchored to the Chinese. *)
nuanceSystem = "You are a classical Chinese philologist with deep familiarity \
with the Daodejing and its history of European translation. The user gives \
you ONE chapter of the Daodejing rendered three ways: the Chinese source \
(Wang Bi recension), James Legge's 1891 translation (the canonical Victorian \
sinologist), and a careful modern English rendering. Articulate the NUANCES \
between Legge and the modern rendering, anchored to the Chinese. Focus on \
abstract-noun choices (\[Dao], \[De], \[Ming], \[Wu]/\[You], \[Xuan]), Victorian register markers \
(parentheticals like '(the idea of)', archaic phrasings such as 'enduring \
and unchanging'), and any places where Legge's Christian conceptual framework \
shows through ('Originator', 'Mystery' with capital M). Be concrete and \
falsifiable. Quote the Chinese character when it matters. 180\[Dash]260 words.";

key = TPFramework`OpenAIKey[];

processChapter[c_Association] := Module[{modern, vecs, dPL, dPM, dLM, prompt, raw, nuance},
   Print["\n=== Daodejing ch. ", c["ch"], " ==="];
   Print["  GPT-5.5 modern translation ..."];
   modern = callTranslate[c["chinese"], c["ch"]];
   If[modern === $Failed, Return[$Failed]];
   modern = StringTrim[modern];
   Print["  modern: ", StringTake[modern, UpTo[100]], "..."];

   Print["  embedding (Chinese + Legge + Modern) ..."];
   vecs = TPFramework`EmbedTexts[{c["chinese"], c["legge"], modern},
      TPFramework`EmbeddingProvider -> "openai"];
   If[vecs === $Failed, Return[$Failed]];
   dPL = CosineDistance[vecs[[1]], vecs[[2]]];  (* zh vs Legge - cross-lang *)
   dPM = CosineDistance[vecs[[1]], vecs[[3]]];  (* zh vs Modern - cross-lang *)
   dLM = CosineDistance[vecs[[2]], vecs[[3]]];  (* Legge vs Modern - within-lang, the real signal *)
   Print["  d(Chinese, Legge)  = ", NumberForm[dPL, {4,3}]];
   Print["  d(Chinese, Modern) = ", NumberForm[dPM, {4,3}]];
   Print["  d(Legge,   Modern) = ", NumberForm[dLM, {4,3}], "  <-- within-English"];

   Print["  GPT-5.5 nuance analysis ..."];
   prompt = StringJoin[
     "Chapter: Daodejing ", ToString[c["ch"]], "\n",
     "Chinese (Wang Bi): ", c["chinese"], "\n",
     "Legge (1891): ", c["legge"], "\n",
     "Modern (you): ", modern, "\n",
     "Cosine distance Legge vs Modern (text-embedding-3-large, English): ",
     ToString[NumberForm[dLM, {4,3}]], "\n\n",
     "Return a single JSON object (no surrounding text):\n",
     "{\n",
     "  \"headline\": \"<one sentence: the single most interesting Legge-vs-Modern nuance>\",\n",
     "  \"abstract_noun\": \"<the Chinese character / Pinyin term the nuance hinges on, or empty>\",\n",
     "  \"victorian_marker\": \"<the Victorian English habit Legge exhibits here, or empty>\",\n",
     "  \"christianising\": \"<if Legge imports Christian vocabulary, name it; else empty>\",\n",
     "  \"explanation\": \"<180\[Dash]260 words of substantive philological commentary>\"\n",
     "}"];
   raw = TPFramework`Private`openaiCall[prompt, "gpt-5.5", 8000, key,
      "high", nuanceSystem, False];
   If[raw === $Failed, Return[$Failed]];
   nuance = TPFramework`Private`parseJSONLoose[raw];

   Append[c, <|"modern" -> modern, "d_zh_legge" -> dPL,
               "d_zh_modern" -> dPM, "d_legge_modern" -> dLM,
               "nuance" -> nuance|>]
];

results = processChapter /@ chapters;
results = Select[results, AssociationQ];

outJSON = FileNameJoin[{repoRoot, "data", "tao_2way.json"}];
Export[outJSON, results, "JSON"];
Print["\nwrote ", outJSON];

(* Distance figure: 3 bars per chapter — the within-English distance
   should be smaller than the cross-language ones, illustrating again
   that the same-language route is where the meaningful signal lives. *)
groupedBars = Table[
   {results[[i]]["d_zh_legge"], results[[i]]["d_zh_modern"],
    results[[i]]["d_legge_modern"]},
   {i, Length[results]}];
chart = BarChart[groupedBars,
   ChartLayout -> "Grouped",
   ChartStyle -> {RGBColor[0.60, 0.25, 0.25], RGBColor[0.70, 0.40, 0.20],
                  RGBColor[0.25, 0.55, 0.40]},
   ChartLegends -> Placed[
      {Style["Chinese vs Legge",         FontFamily -> "Helvetica", FontSize -> 12],
       Style["Chinese vs Modern",         FontFamily -> "Helvetica", FontSize -> 12],
       Style["Legge vs Modern (English)", FontFamily -> "Helvetica", FontSize -> 12]},
      Right],
   ChartLabels -> {Placed[
      Style["Ch. " <> ToString[#["ch"]], FontFamily -> "Helvetica",
            FontSize -> 13, FontColor -> GrayLevel[0.3]] & /@ results,
      Below], None},
   LabelingFunction -> (Placed[
      Style[NumberForm[#, {4, 3}], FontFamily -> "Helvetica",
            FontSize -> 9, FontColor -> GrayLevel[0.3]],
      Above] &),
   Frame -> True, GridLines -> Automatic,
   FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> "Helvetica",
                           FontSize -> 12],
   FrameLabel -> {None, Style["cosine distance (text-embedding-3-large)",
                              FontFamily -> "Helvetica", FontSize -> 14]},
   PlotLabel -> Style[
      "Daodejing \[Dash] cross-language vs within-English divergence",
      Bold, 15, FontFamily -> "Helvetica"],
   ImageSize -> 1200, AspectRatio -> 0.45];
Export[FileNameJoin[{repoRoot, "docs", "images", "tao_distance.png"}], chart];
Print["wrote tao_distance.png"];
