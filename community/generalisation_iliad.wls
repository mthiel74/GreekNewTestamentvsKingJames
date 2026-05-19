#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Three-way generalisation: Homer's Iliad Book 1.
        - Greek (Homeric Greek, standard text)
        - Pope (1715-20 couplets, public domain)
        - Butler (1898 prose, public domain)

    For each aligned passage:
       1. GPT-5.5 (reasoning_effort=high) produces a careful modern
          English translation of the Greek.
       2. Embed all four (Greek, Pope, Butler, modern) with
          text-embedding-3-large.
       3. Compute the 4x4 cosine-distance matrix and the within-
          English distances (Pope vs Butler vs modern).
       4. GPT-5.5 articulates the three-way nuance, comparing Pope's
          neoclassical couplets and Butler's Victorian prose against
          a fresh modern rendering of the same Greek.

    All texts are hardcoded inline so the script does not depend on
    parsing brittle Project Gutenberg layouts. They are public-domain
    snippets, ~20 lines each, easily verified against any standard
    edition.

    Output:
        data/iliad_3way.json
        docs/images/iliad_distance_matrix.png

    Usage:
        wolframscript -file wolfram/generalisation_iliad.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];
Needs["TPFramework`"];

(* ====================================================================
   Source texts loaded from data/iliad_passages.json. They live there
   so wolframscript does not have to read non-ASCII characters out of
   the .wls source (it defaults to Latin-1 on macOS, which double-
   encodes Greek). Import["...", "RawJSON"] handles UTF-8 correctly.
   ==================================================================== *)
passages = Import[FileNameJoin[{repoRoot, "data", "iliad_passages.json"}],
   "RawJSON"];

(* ====================================================================
   Pipeline: translate Greek, embed all four, compute distance matrix,
   ask GPT-5.5 for a three-way nuance comparison.
   ==================================================================== *)

key = TPFramework`OpenAIKey[];

translateAndCompare[p_Association] := Module[
   {modern, vecs, labels, dmat, prompt, raw, nuance},
   Print["\n--- ", p["ref"], " ---"];
   Print["  translating Greek with GPT-5.5 ..."];
   modern = TPFramework`LLMTranslate[p["greek"], p["ref"]];
   If[modern === $Failed, Return[$Failed]];
   Print["  modern: ", StringTake[modern, UpTo[80]], "..."];

   labels = {"greek", "pope", "butler", "modern"};
   Print["  embedding four versions ..."];
   vecs = TPFramework`EmbedTexts[{p["greek"], p["pope"], p["butler"], modern},
      TPFramework`EmbeddingProvider -> "openai"];
   If[vecs === $Failed, Return[$Failed]];
   dmat = Table[CosineDistance[vecs[[i]], vecs[[j]]],
      {i, Length[vecs]}, {j, Length[vecs]}];
   Print["  pairwise distances (English-only):"];
   Print["    Pope    vs Butler: ", NumberForm[dmat[[2,3]], {4,3}]];
   Print["    Pope    vs Modern: ", NumberForm[dmat[[2,4]], {4,3}]];
   Print["    Butler  vs Modern: ", NumberForm[dmat[[3,4]], {4,3}]];

   Print["  asking GPT-5.5 for three-way nuance ..."];
   prompt = StringJoin[
     "You are a classical philologist writing for an academic audience ",
     "(classics, comparative literature). The user gives you ONE passage of ",
     "Homer's Iliad rendered four ways: the Homeric Greek, Alexander Pope's ",
     "1715-20 heroic-couplet translation, Samuel Butler's 1898 prose ",
     "translation, and a careful modern English rendering you just produced. ",
     "Articulate the THREE-WAY nuances between the three English versions, ",
     "anchored to the Greek. Be concrete and falsifiable. Quote the Greek lemma ",
     "(in Greek script) when it matters. Address: (a) Pope's neoclassical ",
     "expansion/personification \[Dash] where does he add a phrase the Greek does ",
     "not contain? (b) Butler's prose decisions \[Dash] where does he flatten an ",
     "epic formula? (c) what the cosine distance numbers (provided) corroborate ",
     "vs reveal. 180\[Dash]260 words.\n\n",
     "Passage: ", p["ref"], "\n",
     "Greek (Homer): ", p["greek"], "\n",
     "Pope (1715-20): ", p["pope"], "\n",
     "Butler (1898): ", p["butler"], "\n",
     "Modern (you): ", modern, "\n",
     "Pope vs Butler distance: ", ToString @ NumberForm[dmat[[2,3]], {4,3}],
     "; Pope vs Modern: ", ToString @ NumberForm[dmat[[2,4]], {4,3}],
     "; Butler vs Modern: ", ToString @ NumberForm[dmat[[3,4]], {4,3}], "\n\n",
     "Reply with a single JSON object (no surrounding text, no markdown fences):\n",
     "{\n",
     "  \"headline\": \"<one sentence on the most striking three-way difference>\",\n",
     "  \"pope_signature\": \"<one or two sentences: Pope's characteristic move on this passage>\",\n",
     "  \"butler_signature\": \"<one or two sentences: Butler's characteristic move>\",\n",
     "  \"greek_lemma\": \"<the Greek word the comparison hinges on (Greek script), or empty>\",\n",
     "  \"explanation\": \"<180\[Dash]260 words of substantive classical-philological commentary>\"\n",
     "}"];
   raw = openaiCallExt[prompt, key];
   If[raw === $Failed, Return[$Failed]];
   nuance = parseJSONLooseExt[raw];

   Append[p, <|"modern" -> modern, "dmat" -> dmat,
               "labels" -> labels, "nuance" -> nuance, "raw" -> raw|>]
];

(* Convenience wrappers that re-export framework internals. *)
openaiCallExt[prompt_, key_] :=
   TPFramework`Private`openaiCall[prompt, "gpt-5.5", 8000, key, "high",
      None, False];
parseJSONLooseExt[s_] := TPFramework`Private`parseJSONLoose[s];

results = translateAndCompare /@ passages;
results = Select[results, AssociationQ];

(* --- save the JSON --- *)
outJSON = FileNameJoin[{repoRoot, "data", "iliad_3way.json"}];
(* Strip the unwieldy raw field for the committed JSON; keep just
   the structured fields. *)
slim = Map[KeyDrop[#, "raw"] &, results];
slim = Map[Append[#, "dmat" -> Normal[#["dmat"]]] &, slim];
Export[outJSON, slim, "JSON"];
Print["\nwrote ", outJSON];

(* --- distance figure: 3 grouped bars, one group per passage --- *)
groupedBars = Table[
   {results[[i]]["dmat"][[2,3]],
    results[[i]]["dmat"][[2,4]],
    results[[i]]["dmat"][[3,4]]},
   {i, Length[results]}];
chart = BarChart[
   Map[Tooltip[#, ToString @ NumberForm[#, {4, 3}]] &, groupedBars, {2}],
   ChartLayout -> "Grouped",
   ChartStyle -> {RGBColor[0.65, 0.25, 0.20], RGBColor[0.15, 0.35, 0.65],
                  RGBColor[0.25, 0.55, 0.40]},
   ChartLegends -> Placed[
      {Style["Pope vs Butler", FontFamily -> "Helvetica", FontSize -> 12],
       Style["Pope vs Modern", FontFamily -> "Helvetica", FontSize -> 12],
       Style["Butler vs Modern", FontFamily -> "Helvetica", FontSize -> 12]},
      Right],
   ChartLabels -> {Placed[
      Style[#["ref"], FontFamily -> "Helvetica", FontSize -> 11,
            FontColor -> GrayLevel[0.3]] & /@ results,
      Below], None},
   (* Value labels above each bar *)
   ChartElementFunction -> ChartElementDataFunction["FadingRectangle",
      "BarColor" -> RGBColor[0.65, 0.25, 0.20]],
   LabelingFunction -> (Placed[
      Style[NumberForm[#, {4, 3}], FontFamily -> "Helvetica",
            FontSize -> 9, FontColor -> GrayLevel[0.3]],
      Above] &),
   Frame -> True, GridLines -> Automatic,
   FrameStyle -> Directive[GrayLevel[0.4], FontFamily -> "Helvetica",
                           FontSize -> 12],
   FrameLabel -> {None, Style["cosine distance (English)",
                              FontFamily -> "Helvetica", FontSize -> 14]},
   PlotLabel -> Style["Iliad Book 1 \[Dash] three-way translation divergence",
                      Bold, 15, FontFamily -> "Helvetica"],
   ImageSize -> 1200, AspectRatio -> 0.42];
Export[FileNameJoin[{repoRoot, "docs", "images", "iliad_distance_matrix.png"}],
   chart];
Print["wrote iliad_distance_matrix.png"];
Print["\ndone (", Length[results], " passages)"];
