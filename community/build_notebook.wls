#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Build community/translation_pairs.nb \[LongDash] the long-form Wolfram
    Community submission for the translation-pair framework.

    Architecture lifted from ENSO-emergence/community/build_notebook.wls:
    open an invisible front-end notebook via UsingFrontEnd /
    CreateDocument, CellPrint fully-formed ExpressionCells into it.

    Sections
    --------
      Abstract
      1. Motivation \[LongDash] translation as a measurable thing
      2. The headline pair: SBLGNT (Koine Greek NT) vs the KJV
         2.1 Sources and licences
         2.2 Verse counts and the manuscript-variant set
      3. The framework
         3.1 Alignment
         3.2 Multilingual embeddings
         3.3 Cosine distance
         3.4 LLM hypothesis generation
      4. Results: where the KJV drifts hardest
         4.1 Distance distribution
         4.2 Per-book divergence
         4.3 Top-50 divergent verses with LLM tags
         4.4 Manuscript-variant verses (Greek-absent)
      5. Generalisation
         5.1 KJV vs Luther 1545
         5.2 Iliad: Pope vs Butler
         5.3 Marx's Kapital, Tao Te Ching
      6. Discussion
      7. References & acknowledgements
      8. Reproducibility

    The build is robust against missing figures (the analyze.wls
    step produces them; if it has not run yet, placeholders are
    inserted so the .nb still compiles for review).
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
imgDir   = FileNameJoin[{repoRoot, "docs", "images"}];
dataDir  = FileNameJoin[{repoRoot, "data"}];
commDir  = FileNameJoin[{repoRoot, "community"}];
outNb    = FileNameJoin[{commDir, "translation_pairs.nb"}];
outPdf   = FileNameJoin[{commDir, "translation_pairs.pdf"}];

(* Keep the WL packages AND the .wls helper scripts the notebook
   depends on in sync next to the notebook itself, so users who
   download community/ get one self-contained bundle: notebook +
   every script its cells refer to. NotebookDirectory[] then resolves
   to the same folder for every Get / wolframscript invocation. *)
Do[
   With[{src = FileNameJoin[{repoRoot, "wolfram", f}],
         dst = FileNameJoin[{commDir, f}]},
     If[FileExistsQ[src],
        CopyFile[src, dst, OverwriteTarget -> True];
        Print["  synced ", f, " \[RightArrow] community/"]]],
   {f, {
      (* core packages *)
      "framework.wl", "translator_tool.wl", "fetcher_common.wl",
      (* fetchers *)
      "fetch_kjv.wls", "fetch_greek_nt.wls",
      "fetch_historical_bibles.wls", "fetch_extra_pairs.wls",
      (* analyses *)
      "section_analysis.wls", "embed.wls", "analyze.wls",
      "generalisation_iliad.wls", "generalisation_tao.wls",
      "embedding_comparison.wls", "extra_visualisations.wls",
      "translation_drift.wls", "length_vs_divergence.wls",
      "news_fact_checker.wls", "redraw_section_charts.wls",
      (* tool examples + image generation *)
      "run_tool_example.wls", "generate_illustrations.wls",
      "nuance_only.wls", "retranslate_truncated.wls"}}];

(* Pull GPT-5.5 nuance analysis for each worked case study. The
   build is tolerant: any section without a *_nuance.json file
   renders as a placeholder. *)
loadSection[key_] := With[{
   t = FileNameJoin[{dataDir, "section_" <> key <> "_translated.json"}],
   n = FileNameJoin[{dataDir, "section_" <> key <> "_nuance.json"}]},
   <|"translated" -> If[FileExistsQ[t], Import[t, "RawJSON"], {}],
     "nuance"     -> If[FileExistsQ[n], Import[n, "RawJSON"], {}]|>];

romans8 = loadSection["romans-8"];
cor13   = loadSection["1-corinthians-13"];

(* Iliad / Tao are 3-way and 3-way generalisations \[Dash] one JSON file each,
   loaded raw. *)
iliadFile = FileNameJoin[{dataDir, "iliad_3way.json"}];
iliadRows = If[FileExistsQ[iliadFile], Import[iliadFile, "RawJSON"], {}];
taoFile = FileNameJoin[{dataDir, "tao_2way.json"}];
taoRows = If[FileExistsQ[taoFile], Import[taoFile, "RawJSON"], {}];

(* Translator-tool frozen example. *)
toolFile = FileNameJoin[{dataDir, "translator_tool_example.json"}];
toolExample = If[FileExistsQ[toolFile],
   Import[toolFile, "RawJSON"], None];

(* News fact-checker case studies (§10). *)
nfcFile = FileNameJoin[{dataDir, "news_fact_checker.json"}];
nfcCases = If[FileExistsQ[nfcFile],
   Import[nfcFile, "RawJSON"], {}];

(* Generated illustration paths. *)
illustrationDir = FileNameJoin[{imgDir, "figures-generated"}];

(* Backwards compat with the older single-section build. *)
nuanceRows = romans8["nuance"];

(* Verse exhibit: Greek block + KJV block + modern + GPT's nuance prose.
   Renders nothing if the data isn't there yet. *)
greekStyle  = {FontFamily -> "Times", FontSize -> 14, FontSlant -> Italic,
   FontColor -> RGBColor[0.10, 0.20, 0.50]};
kjvStyle    = {FontFamily -> "Times", FontSize -> 13, FontSlant -> Italic,
   FontColor -> RGBColor[0.20, 0.10, 0.05]};
modernStyle = {FontFamily -> "Helvetica", FontSize -> 13,
   FontColor -> GrayLevel[0.15]};
bodyStyle   = {FontFamily -> "Helvetica", FontSize -> 12,
   FontColor -> GrayLevel[0.2]};
labelStyle  = {FontFamily -> "Helvetica", FontSize -> 12,
   FontWeight -> Bold, FontColor -> GrayLevel[0.35]};

(* A faint rule between verse exhibits to give visual rhythm. *)
verseDivider = Cell[
   BoxData @ ToBoxes @ Graphics[{
      GrayLevel[0.8], Thickness[0.0015],
      Line[{{0, 0}, {1, 0}}]},
      AspectRatio -> 1/120, ImageSize -> 480, ImagePadding -> 0],
   "Output", CellMargins -> {{120, 120}, {2, 8}}];

getField[row_, path__] := Module[{r = row},
   Do[r = If[AssociationQ[r] && KeyExistsQ[r, p], r[p], ""], {p, {path}}];
   If[StringQ[r] || ListQ[r], r, ""]];

verseExhibit[row_Association] := Module[
   {tag, headline, key, drift, textual, crossRefs, body, refsLine},
   tag       = getField[row, "nuance", "category"]; If[tag === "", tag = "other"];
   headline  = getField[row, "nuance", "primary_nuance"];
   key       = getField[row, "nuance", "key_term"];
   drift     = getField[row, "nuance", "english_drift"];
   textual   = getField[row, "nuance", "textual_note"];
   crossRefs = getField[row, "nuance", "cross_refs"];
   body      = getField[row, "nuance", "explanation"];
   refsLine  = If[ListQ[crossRefs] && crossRefs =!= {},
      StringRiffle[crossRefs, " \[Bullet] "], ""];
   {
     verseDivider,
     Cell[TextData[{
        StyleBox[row["ref"], FontWeight -> Bold, FontSize -> 16,
                 FontColor -> RGBColor[0.7, 0.2, 0.2]],
        "   \[Bullet]   ",
        StyleBox["d = " <> ToString @ NumberForm[row["distance"], {4, 3}],
                 FontFamily -> "Courier", FontSize -> 12,
                 FontColor -> GrayLevel[0.4]],
        "   \[Bullet]   ",
        StyleBox[tag, FontFamily -> "Courier", FontSize -> 12,
                 Background -> RGBColor[0.95, 0.92, 0.82]],
        If[key =!= "",
          Sequence @@ {"   \[Bullet]   ",
            StyleBox[key, FontFamily -> "Times", FontSize -> 14,
                     FontSlant -> Italic,
                     FontColor -> RGBColor[0.10, 0.20, 0.50]]},
          Sequence @@ {}]
     }], "Text",
       CellMargins -> {{60, 60}, {14, 6}}],
     Cell[TextData[{StyleBox["Greek (SBLGNT): ", Sequence @@ labelStyle],
                    StyleBox[row["greek"], Sequence @@ greekStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["KJV (1769): ", Sequence @@ labelStyle],
                    StyleBox[row["kjv"], Sequence @@ kjvStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Modern (GPT-5.5): ", Sequence @@ labelStyle],
                    StyleBox[row["modern"], Sequence @@ modernStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 8}}],
     If[headline =!= "",
       Cell[TextData[{StyleBox["Headline: ", Sequence @@ labelStyle,
                        FontColor -> RGBColor[0.4,0.3,0.1]],
                      StyleBox[headline, Sequence @@ bodyStyle]}],
         "Text", CellMargins -> {{80, 60}, {2, 2}}],
       Nothing],
     If[drift =!= "",
       Cell[TextData[{StyleBox["English drift: ", Sequence @@ labelStyle],
                      StyleBox[drift, Sequence @@ bodyStyle]}],
         "Text", CellMargins -> {{80, 60}, {2, 2}}],
       Nothing],
     If[textual =!= "",
       Cell[TextData[{StyleBox["Textual note: ", Sequence @@ labelStyle],
                      StyleBox[textual, Sequence @@ bodyStyle]}],
         "Text", CellMargins -> {{80, 60}, {2, 2}}],
       Nothing],
     If[refsLine =!= "",
       Cell[TextData[{StyleBox["Cross-refs: ", Sequence @@ labelStyle],
                      StyleBox[refsLine, FontFamily -> "Courier",
                               FontSize -> 12, FontColor -> GrayLevel[0.3]]}],
         "Text", CellMargins -> {{80, 60}, {2, 2}}],
       Nothing],
     If[body =!= "",
       Cell[TextData[{StyleBox["GPT-5.5 (reasoning_effort=high): ",
                        Sequence @@ labelStyle],
                      StyleBox[body, Sequence @@ bodyStyle]}],
         "Text", CellMargins -> {{80, 60}, {2, 12}}],
       Nothing]
   }];

(* ====================================================================
   Iliad-row renderer (3-way: Greek + Pope + Butler + GPT modern).
   ==================================================================== *)
iliadExhibit[row_Association] := Module[
   {dmat, dPB, dPM, dBM, n, h, p, b, g, e},
   dmat = row["dmat"];
   dPB = dmat[[2,3]]; dPM = dmat[[2,4]]; dBM = dmat[[3,4]];
   n = row["nuance"];
   h = If[AssociationQ[n], getField[row, "nuance", "headline"], ""];
   p = If[AssociationQ[n], getField[row, "nuance", "pope_signature"], ""];
   b = If[AssociationQ[n], getField[row, "nuance", "butler_signature"], ""];
   g = If[AssociationQ[n], getField[row, "nuance", "greek_lemma"], ""];
   e = If[AssociationQ[n], getField[row, "nuance", "explanation"], ""];
   {
     verseDivider,
     Cell[TextData[{
        StyleBox[row["ref"], FontWeight -> Bold, FontSize -> 16,
                 FontColor -> RGBColor[0.5, 0.2, 0.4]],
        If[g =!= "", Sequence @@ {"   \[Bullet]   ",
           StyleBox[g, Sequence @@ greekStyle]}, Sequence @@ {}]
     }], "Text", CellMargins -> {{60, 60}, {14, 6}}],
     Cell[TextData[{StyleBox["Greek (Homer): ", Sequence @@ labelStyle],
                    StyleBox[row["greek"], Sequence @@ greekStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Pope (1715-20): ", Sequence @@ labelStyle],
                    StyleBox[row["pope"], Sequence @@ kjvStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Butler (1898): ", Sequence @@ labelStyle],
                    StyleBox[row["butler"], Sequence @@ kjvStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Modern (GPT-5.5): ", Sequence @@ labelStyle],
                    StyleBox[row["modern"], Sequence @@ modernStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 6}}],
     Cell[TextData[{StyleBox["Distances: ", Sequence @@ labelStyle],
        StyleBox["Pope/Butler = " <> ToString @ NumberForm[dPB, {4,3}] <>
                 "    Pope/Modern = " <> ToString @ NumberForm[dPM, {4,3}] <>
                 "    Butler/Modern = " <> ToString @ NumberForm[dBM, {4,3}],
                 FontFamily -> "Courier", FontSize -> 12,
                 FontColor -> GrayLevel[0.3]]}],
       "Text", CellMargins -> {{80, 60}, {2, 6}}],
     If[h =!= "", Cell[TextData[{StyleBox["Headline: ", Sequence @@ labelStyle,
                        FontColor -> RGBColor[0.4,0.3,0.1]],
                      StyleBox[h, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[p =!= "", Cell[TextData[{StyleBox["Pope's signature: ",
                        Sequence @@ labelStyle],
                      StyleBox[p, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[b =!= "", Cell[TextData[{StyleBox["Butler's signature: ",
                        Sequence @@ labelStyle],
                      StyleBox[b, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[e =!= "", Cell[TextData[{StyleBox["GPT-5.5: ", Sequence @@ labelStyle],
                      StyleBox[e, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 12}}], Nothing]
   }];

(* ====================================================================
   Tao-row renderer (3-way Chinese + Legge + GPT modern).
   ==================================================================== *)
taoExhibit[row_Association] := Module[{n, h, ab, vm, ch, e, label},
   n = row["nuance"];
   label = "Daodejing ch. " <> ToString[row["ch"]];
   h  = If[AssociationQ[n], getField[row, "nuance", "headline"], ""];
   ab = If[AssociationQ[n], getField[row, "nuance", "abstract_noun"], ""];
   vm = If[AssociationQ[n], getField[row, "nuance", "victorian_marker"], ""];
   ch = If[AssociationQ[n], getField[row, "nuance", "christianising"], ""];
   e  = If[AssociationQ[n], getField[row, "nuance", "explanation"], ""];
   {
     verseDivider,
     Cell[TextData[{
        StyleBox[label, FontWeight -> Bold, FontSize -> 16,
                 FontColor -> RGBColor[0.2, 0.5, 0.4]],
        If[ab =!= "", Sequence @@ {"   \[Bullet]   ",
           StyleBox[ab, FontFamily -> "Times", FontSize -> 16,
              FontColor -> RGBColor[0.20, 0.40, 0.10]]}, Sequence @@ {}]
     }], "Text", CellMargins -> {{60, 60}, {14, 6}}],
     Cell[TextData[{StyleBox["Chinese: ", Sequence @@ labelStyle],
                    StyleBox[row["chinese"], FontFamily -> "Times",
                       FontSize -> 14,
                       FontColor -> RGBColor[0.20, 0.40, 0.10]]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Legge (1891): ", Sequence @@ labelStyle],
                    StyleBox[row["legge"], Sequence @@ kjvStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 2}}],
     Cell[TextData[{StyleBox["Modern (GPT-5.5): ", Sequence @@ labelStyle],
                    StyleBox[row["modern"], Sequence @@ modernStyle]}],
       "Text", CellMargins -> {{80, 60}, {2, 6}}],
     Cell[TextData[{StyleBox["Distances: ", Sequence @@ labelStyle],
        StyleBox["zh/Legge = " <> ToString @ NumberForm[row["d_zh_legge"], {4,3}] <>
                 "    zh/Modern = " <> ToString @ NumberForm[row["d_zh_modern"], {4,3}] <>
                 "    Legge/Modern = " <> ToString @ NumberForm[row["d_legge_modern"], {4,3}],
                 FontFamily -> "Courier", FontSize -> 12,
                 FontColor -> GrayLevel[0.3]]}],
       "Text", CellMargins -> {{80, 60}, {2, 6}}],
     If[h =!= "", Cell[TextData[{StyleBox["Headline: ", Sequence @@ labelStyle,
                        FontColor -> RGBColor[0.4,0.3,0.1]],
                      StyleBox[h, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[vm =!= "", Cell[TextData[{StyleBox["Victorian marker: ",
                        Sequence @@ labelStyle],
                      StyleBox[vm, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[ch =!= "", Cell[TextData[{StyleBox["Christianising: ",
                        Sequence @@ labelStyle],
                      StyleBox[ch, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 2}}], Nothing],
     If[e =!= "", Cell[TextData[{StyleBox["GPT-5.5: ", Sequence @@ labelStyle],
                      StyleBox[e, Sequence @@ bodyStyle]}],
                      "Text", CellMargins -> {{80, 60}, {2, 12}}], Nothing]
   }];

(* Renderer for the frozen translator-tool example. Shows source,
   baseline, ranked candidates with distances, and per-candidate nuance. *)
toolExampleCells[res_Association] := Module[{cands, commentary, mdl, baseLbl, expLbl},
   cands = res["candidates"];
   commentary = res["commentary"];
   (* Use the model name baked into the saved example, falling back
      cleanly if older snapshots don't carry it. *)
   mdl = If[KeyExistsQ[res, "model"] && StringQ[res["model"]],
            res["model"], "GPT-5.5"];
   baseLbl = mdl <> " baseline (modern English): ";
   expLbl  = mdl <> ": ";
   Join[
     {Cell[TextData[{StyleBox["Source (Koine Greek): ", Sequence @@ labelStyle],
                     StyleBox[res["source"], Sequence @@ greekStyle]}],
        "Text", CellMargins -> {{80, 60}, {4, 2}}],
      Cell[TextData[{StyleBox[baseLbl, Sequence @@ labelStyle],
                     StyleBox[res["baseline"], Sequence @@ modernStyle]}],
        "Text", CellMargins -> {{80, 60}, {2, 10}}],
      Cell[TextData[{StyleBox["Ranked candidates (lowest distance = closest to baseline):",
         FontWeight -> Bold, FontSize -> 13]}],
        "Text", CellMargins -> {{80, 60}, {6, 4}}]},
     Flatten[
       MapIndexed[Function[{c, idx},
         With[{n = commentary[[First[idx]]]},
           {Cell[TextData[{
              StyleBox["Candidate " <> ToString[c["index"]], FontWeight -> Bold,
                 FontSize -> 13, FontColor -> RGBColor[0.4, 0.2, 0.5]],
              "   \[Bullet]   ",
              StyleBox["d = " <> ToString @ NumberForm[c["distance"], {4, 3}],
                 FontFamily -> "Courier", FontSize -> 12,
                 FontColor -> GrayLevel[0.4]]
            }], "Text", CellMargins -> {{80, 60}, {10, 2}}],
            Cell[TextData[{StyleBox["text: ", Sequence @@ labelStyle],
                           StyleBox[c["candidate"], Sequence @@ kjvStyle]}],
              "Text", CellMargins -> {{100, 60}, {2, 2}}],
            If[KeyExistsQ[n, "distinctive_choice"] &&
                StringQ[n["distinctive_choice"]] && n["distinctive_choice"] =!= "",
              Cell[TextData[{StyleBox["distinctive choice: ", Sequence @@ labelStyle],
                             StyleBox[n["distinctive_choice"], Sequence @@ bodyStyle]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "key_lemma"] && StringQ[n["key_lemma"]] && n["key_lemma"] =!= "",
              Cell[TextData[{StyleBox["key lemma: ", Sequence @@ labelStyle],
                             StyleBox[n["key_lemma"], Sequence @@ greekStyle]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "suggested_alt"] && StringQ[n["suggested_alt"]] && n["suggested_alt"] =!= "",
              Cell[TextData[{StyleBox["suggested alternative: ", Sequence @@ labelStyle],
                             StyleBox[n["suggested_alt"],
                               FontFamily -> "Times", FontSlant -> Italic,
                               FontSize -> 13]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "explanation"] && StringQ[n["explanation"]],
              Cell[TextData[{StyleBox[expLbl, Sequence @@ labelStyle],
                             StyleBox[n["explanation"], Sequence @@ bodyStyle]}],
                "Text", CellMargins -> {{100, 60}, {2, 8}}], Nothing]}]],
         cands], 1]
   ]];

(* Renderer for one news fact-checker case (§10).
   Header + headnote, source passage, baseline, ranked candidates
   with their labels, distances, and per-candidate nuance commentary. *)
newsCaseExhibit[c_Association] := Module[{a, labels, ranked, comm, langStyle},
   a       = c["audit"];
   labels  = a["candidate_labels"];
   ranked  = a["candidates"];
   comm    = a["commentary"];
   langStyle = {FontFamily -> "Times", FontSize -> 13, FontSlant -> Italic,
                FontColor -> RGBColor[0.10, 0.20, 0.50]};
   Join[
     {Cell[c["title"], "Subsection"],
      Cell[TextData[c["headnote"]], "Text",
           CellMargins -> {{50, 50}, {6, 8}}],
      Cell[TextData[{StyleBox["Source (" <> c["sourceLang"] <> "): ",
                       Sequence @@ labelStyle],
                     StyleBox[c["source"], Sequence @@ langStyle]}],
        "Text", CellMargins -> {{80, 60}, {4, 4}}],
      Cell[TextData[{StyleBox[a["model"] <> " baseline (independent English): ",
                       Sequence @@ labelStyle],
                     StyleBox[a["baseline"], Sequence @@ modernStyle]}],
        "Text", CellMargins -> {{80, 60}, {2, 12}}],
      Cell[TextData[{StyleBox[
         "Ranked candidates (lowest distance = closest to the baseline):",
         FontWeight -> Bold, FontSize -> 13]}], "Text",
         CellMargins -> {{80, 60}, {6, 4}}]},
     Flatten[
       MapIndexed[Function[{r, idx},
         With[{n = comm[[First[idx]]], lbl = labels[[First[idx]]]},
           {Cell[TextData[{
              StyleBox[lbl, FontWeight -> Bold, FontSize -> 13,
                 FontColor -> RGBColor[0.4, 0.2, 0.5]],
              "   \[Bullet]   ",
              StyleBox["d = " <> ToString @ NumberForm[r["distance"], {4, 3}],
                 FontFamily -> "Courier", FontSize -> 12,
                 FontColor -> GrayLevel[0.4]]
            }], "Text", CellMargins -> {{80, 60}, {10, 2}}],
            Cell[TextData[{StyleBox["text: ", Sequence @@ labelStyle],
                           StyleBox[r["candidate"], Sequence @@ kjvStyle]}],
              "Text", CellMargins -> {{100, 60}, {2, 2}}],
            If[KeyExistsQ[n, "distinctive_choice"] &&
                StringQ[n["distinctive_choice"]] && n["distinctive_choice"] =!= "",
              Cell[TextData[{StyleBox["distinctive choice: ", Sequence @@ labelStyle],
                             StyleBox[n["distinctive_choice"], Sequence @@ bodyStyle]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "key_lemma"] && StringQ[n["key_lemma"]] && n["key_lemma"] =!= "",
              Cell[TextData[{StyleBox["key term: ", Sequence @@ labelStyle],
                             StyleBox[n["key_lemma"], FontFamily -> "Times",
                               FontSlant -> Italic, FontSize -> 13,
                               FontColor -> RGBColor[0.10, 0.20, 0.50]]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "suggested_alt"] && StringQ[n["suggested_alt"]] && n["suggested_alt"] =!= "",
              Cell[TextData[{StyleBox["suggested alternative: ", Sequence @@ labelStyle],
                             StyleBox[n["suggested_alt"],
                               FontFamily -> "Times", FontSlant -> Italic, FontSize -> 13]}],
                "Text", CellMargins -> {{100, 60}, {2, 2}}], Nothing],
            If[KeyExistsQ[n, "explanation"] && StringQ[n["explanation"]],
              Cell[TextData[{StyleBox[a["model"] <> ": ", Sequence @@ labelStyle],
                             StyleBox[n["explanation"], Sequence @@ bodyStyle]}],
                "Text", CellMargins -> {{100, 60}, {2, 12}}], Nothing]}]],
         ranked], 1]
   ]];

(* Helper for inserting one of the gpt-image-2 generated illustrations
   with a caption. Pure WL renders the cell; image is just imported. *)
illustrationCell[fname_, caption_] := Module[{full, im},
   full = FileNameJoin[{illustrationDir, fname}];
   im = If[FileExistsQ[full], Import[full],
      Graphics[{Text[Style["illustration not generated yet:\n" <> fname,
        Italic, GrayLevel[0.5]], {0, 0}]}, ImageSize -> 400]];
   {Cell[BoxData @ ToBoxes @ Show[im, ImageSize -> 560], "Output",
      CellMargins -> {{60, 60}, {6, 4}}, TextAlignment -> Center],
    Cell[TextData[{StyleBox["Figure. ", FontWeight -> Bold], caption}],
      "Caption", CellMargins -> {{80, 80}, {2, 18}},
      TextAlignment -> Center]}];

(* ====================================================================
   Cell-builder helpers
   ==================================================================== *)

title[txt_]    := Cell[txt, "Title"];
subtitle[txt_] := Cell[txt, "Subtitle"];
hd1[txt_]      := Cell[txt, "Section"];
hd2[txt_]      := Cell[txt, "Subsection"];
hd3[txt_]      := Cell[txt, "Subsubsection"];
para[stuff_]   := Cell[TextData[stuff], "Text",
   CellMargins -> {{50, 50}, {6, 6}}];
abstractCell[stuff_] := Cell[TextData[stuff], "Text",
   FontSize -> 14, CellMargins -> {{60, 60}, {6, 6}},
   Background -> RGBColor[0.98, 0.96, 0.90]];

bold[s_] := StyleBox[s, FontWeight -> Bold];
ital[s_] := StyleBox[s, FontSlant -> Italic];
mono[s_] := StyleBox[s, FontFamily -> "Courier"];

(* Image cell with cache + missing-figure placeholder. *)
imgCacheDir = FileNameJoin[{repoRoot, "community", ".img-cache"}];
If[!DirectoryQ[imgCacheDir], CreateDirectory[imgCacheDir]];

resizeFor[path_String] := Module[{im, w, h, target = 1200, cached},
   cached = FileNameJoin[{imgCacheDir, FileNameTake[path]}];
   If[FileExistsQ[cached] &&
      FileDate[cached, "Modification"] > FileDate[path, "Modification"],
     Return[Import[cached]]];
   im = Import[path];
   {w, h} = ImageDimensions[im];
   im = If[w > target, ImageResize[im, target], im];
   Export[cached, im, "PNG"];
   im
];

(* Emit a styled Input cell containing the code that produced the
   image below it. Used in front of every Wolfram-generated figure
   so the notebook reader can see exactly how each output was made
   and (when the script is shipped alongside the notebook) re-run it.

   The cell is a real Input cell: Shift-Enter re-evaluates it. The
   image is loaded from disk so the cell does not need to re-fetch
   API content to display the chart. *)
codeCell[code_String] := Cell[code, "Input", FontFamily -> "Courier",
   FontSize -> 11, CellMargins -> {{60, 60}, {6, 4}}];

(* Convenience: paired (code, image, caption) triplet. *)
codedImageCell[relPath_, code_String, caption_:""] :=
   Sequence @@ Flatten[{
      codeCell[code],
      imageCell[relPath, caption]}];

imageCell[relPath_, caption_:""] := Module[{full, im},
   full = FileNameJoin[{imgDir, relPath}];
   im = If[FileExistsQ[full],
      resizeFor[full],
      Graphics[{
        EdgeForm[GrayLevel[0.6]], FaceForm[GrayLevel[0.95]],
        Rectangle[{0, 0}, {6, 3}],
        Text[Style["figure not built yet:\n" <> relPath <>
              "\nrun wolframscript -file wolfram/analyze.wls",
              Italic, GrayLevel[0.4]], {3, 1.5}]},
        PlotRange -> {{0, 6}, {0, 3}}, ImageSize -> 640]];
   {Cell[BoxData @ ToBoxes @ Show[im, ImageSize -> 620], "Output",
      CellMargins -> {{40, 40}, {4, 4}}, TextAlignment -> Center],
    If[caption =!= "",
      Cell[TextData[{StyleBox["Figure. ", Bold], caption}], "Caption",
        CellMargins -> {{80, 80}, {4, 16}}, TextAlignment -> Center],
      Nothing]}
];

(* ====================================================================
   Section payload
   ==================================================================== *)

cells = {

title["Lost in Translation"],
subtitle["A Wolfram framework for auditing the nuance gap between any two renderings \[LongDash] scripture, news, treaties"],

Sequence @@ illustrationCell["hero_cover.png",
   "Cover. Every translation is a choice. This notebook turns those choices into a number you can rank, a chart you can read, and an LLM commentary you can quote."],

para[{
  "Every Wolfram-generated output in this notebook is reproduced from a single \
companion package, ", mono["lost_in_translation.wl"],
  ", which consolidates the framework, the translator-audit tool, and all \
frozen JSON data. Drop the .wl beside the notebook and evaluate this cell once:"}],
Cell["Get[FileNameJoin[{NotebookDirectory[], \"lost_in_translation.wl\"}]];",
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 8}}],
para[{"Then every code cell in this notebook \[Dash] ", mono["DriftTimeline[]"],
  ", ", mono["Audit[\[Ellipsis]]"], ", and the rest \[Dash] is just a call \
into the ", mono["LostInTranslation`"], " context. The four conceptual \
illustrations were produced once by ", mono["gpt-image-2"],
  " (a single OpenAI image call apiece) and are embedded as PNGs."}],
para[{"Full source code, the unconsolidated ", mono["wolfram/*.wls"],
  " pipeline scripts, all frozen JSON, and the gpt-image-2 PNGs are \
available on GitHub at ",
  Cell[BoxData @ ToBoxes @ Hyperlink[
    "https://github.com/mthiel74/GreekNewTestamentvsKingJames",
    "https://github.com/mthiel74/GreekNewTestamentvsKingJames"]],
  "."}],

abstractCell[{
  "Whenever a text crosses a language, a translator makes choices: which sense of a polysemous word to pick, which idiom to flatten, which manuscript variant to follow, how much rhetorical decoration to add. Most of the time those choices don't matter. Sometimes they do \[Dash] a theological loading, a manuscript variant, a journalistic spin. Reading two translations side by side is the oldest technique in philology for catching them; it doesn't scale. ",
  "This notebook builds a small, language-agnostic framework that takes any two renderings of the same source, embeds each one with a multilingual model, ranks aligned units by cosine distance, and asks a frontier LLM to articulate \[Dash] in concrete, falsifiable prose with the source-language lemma quoted \[Dash] ", ital["why"], " each large gap exists. ",
  "The headline case study is the SBLGNT vs the KJV. The same pipeline then runs on Homer's ", ital["Iliad"],
  " (Pope vs Butler), the ", ital["Daodejing"],
  " (Legge 1891 vs a fresh modern rendering), and \[Dash] in \[Section] 10 \[Dash] on three multilingual news / treaty texts where the framework reads as a translation ", bold["fact-checker"], ": the UDHR, a Vatican encyclical, and a UN Secretary-General address. An interactive ", mono["LostInTranslation`Audit[\[Ellipsis]]"],
  " function lets you run the same audit on any text in any source language."}],

hd1["1. Motivation \[LongDash] translation as a measurable thing"],
para[{
  "Reading two translations side by side is the oldest technique in ",
  "philology. It scales badly. A multilingual sentence embedder ",
  "(Cohere's ", mono["embed-multilingual-v3.0"], ", OpenAI's ",
  mono["text-embedding-3-large"], ", or Voyage's ", mono["voyage-3"],
  ") gives us a single ~1024-dim vector per verse, in a shared space ",
  "across the two source languages. Cosine distance is then a single ",
  "scalar that compresses 'how far has the translation moved' into ",
  "something we can rank, plot, and feed to an LLM that has read more ",
  "commentary than any of us."}],

para[{
  "A preview of what the rest of the notebook will do, in one chart. ",
  "Six public-domain English translations of the New Testament, sorted by ",
  "publication year, all rendering the same Romans 8. The y-axis is the ",
  "cosine distance from each translation to ", bold["Tyndale 1534"],
  " (the baseline, distance 0). What the chart says, very loud: ",
  "almost all the lexical/stylistic drift happens between Tyndale (1534) ",
  "and the KJV (1611). After 1611, every modern translation hovers ",
  "around the same cumulative distance \[Dash] they all stand together ",
  "against the Reformation-era English."}],
codeCell["LostInTranslation`DriftTimeline[]"],
Sequence @@ imageCell["drift_timeline.png",
  "Cumulative cosine drift from Tyndale 1534 across six public-domain English NT translations on Romans 8 (text-embedding-3-large; chapter-level embedding). The 1611 KJV is where four centuries of English changed gear."],

hd1["2. The headline pair: SBLGNT vs the KJV"],
hd2["2.1 Sources and licences"],
para[{
  "The Greek side is the ", bold["SBL Greek New Testament"],
  " (Holmes, 2010), distributed under CC BY 4.0 via the ",
  mono["morphgnt/sblgnt"], " GitHub repository. The English side is the ",
  bold["1769 Cambridge edition of the KJV"], " (public domain in the US) ",
  "via the ", mono["aruljohn/Bible-kjv"], " repository. Both sources are ",
  "fetched by the project's ", mono["wolfram/fetch_*.wls"], " scripts; ",
  "see ", mono["docs/sources.md"], " for full citation strings."}],

hd2["2.2 Verse counts and the manuscript-variant set"],
para[{
  "After fetching, the KJV NT has ", bold["7 957 verses"],
  " and the SBLGNT has ", bold["7 927 verses"], ". The 30-verse asymmetry ",
  "is itself a piece of useful signal: it is exactly the set of verses ",
  "the KJV's underlying Textus Receptus contains and the SBLGNT does ",
  "not. The framework's ", mono["LoadPair"], " function surfaces these as ",
  mono["onlyB"], " \[LongDash] they include ", ital["Acts 8:37"],
  " (the Ethiopian eunuch's confession), the longer ending of ",
  ital["Mark"], ", and the pericope adulterae fragments in ", ital["John 7-8"], "."}],

hd1["3. The framework"],
Sequence @@ illustrationCell["pipeline_diagram.png",
  "The four-stage pipeline. Steps 1\[Dash]4 are described in detail in \[Section] 3.1\[Dash]3.4."],

hd2["3.1 Alignment"],
para[{
  "For Bible-shaped pairs we inherit the canonical Book / Chapter / Verse ",
  "axis as the join key. For prose pairs (Pope vs Butler ", ital["Iliad"],
  ", Marx in two languages, two ", ital["Tao Te Ching"], " versions) we ",
  "fall back to a sentence-level dynamic-programming aligner over ",
  "length ratios \[LongDash] same trick as Gale-Church (1991), updated to use ",
  "cosine distance between cheap embeddings as the alignment cost. ",
  "See ", mono["LoadPair"], " inside ", mono["LostInTranslation`"], "."}],
hd2["3.2 Embedding \[LongDash] and why we don't embed across languages"],
Sequence @@ illustrationCell["embedding_space.png",
  "Off-the-shelf multilingual embedders cluster more strongly by language than by meaning. \
The actionable signal lives in within-language distance \[Dash] hence the translate-then-compare detour."],
para[{
  "A naive pipeline would embed the Greek and the English directly and ",
  "compute a cross-language distance. We tried that. Off-the-shelf ",
  "multilingual embedders \[LongDash] including ",
  mono["text-embedding-3-large"],
  " \[LongDash] cluster more strongly by ",
  ital["language"], " than by ", ital["meaning"], ": even a perfect ",
  "Greek/English verse pair sits at cosine distance \[TildeTilde] 0.85-0.90. ",
  "The signal is buried."}],
para[{
  "The fix: ", bold["have GPT-5.5 produce a careful modern English ",
                    "translation of each Greek verse"],
  ", then embed the KJV and the modern translation together. Same-language ",
  "embedding is what these models do well; cosine distance now reflects ",
  ital["semantic"], " divergence between two English renderings of the same ",
  "Greek source. Per the user's note: the gross meaning will of course be ",
  "similar; what we are looking for is the ",
  ital["nuance"], " \[LongDash] the small but real lexical, theological or ",
  "textual choice the KJV makes that a modern translator would make ",
  "differently."}],
hd2["3.3 Cosine distance"],
para[{
  "On the same-language route we compute ",
  Cell[BoxData @ FormBox[
     RowBox[{"d", "(", "a", ",", "b", ")", "=", "1", "-",
       FractionBox[RowBox[{"a", "\[CenterDot]", "b"}],
                   RowBox[{"\[LeftDoubleBracketingBar]", "a", "\[RightDoubleBracketingBar]",
                           "\[CenterDot]",
                           "\[LeftDoubleBracketingBar]", "b", "\[RightDoubleBracketingBar]"}]]}],
     TraditionalForm], "InlineFormula"],
  " using the built-in ", mono["CosineDistance"],
  ". On Romans 8 the distribution sits roughly in [0.05, 0.45]; the ",
  "right tail starts at \[TildeTilde] 0.25 and is where the interesting ",
  "verses live."}],
hd2["3.4 Nuance analysis (GPT-5.5 with high reasoning effort)"],
Sequence @@ illustrationCell["nuance_taxonomy.png",
  "The seven-category rubric GPT-5.5 is asked to choose from. Tags are not ground truth \
but a quick way to cluster which kind of drift dominates a chapter."],
para[{
  "For the top-N most-divergent verses in the section we hand GPT-5.5 the ",
  "Greek, the KJV, its own modern translation, and the cosine distance, and ",
  "ask it to articulate the ", ital["nuances"],
  " (not the gross meaning, which is similar by construction). The system ",
  "prompt tells it to skip trivial differences \[LongDash] \"thou\" vs ",
  "\"you\", \"begat\" vs \"fathered\" \[LongDash] and focus on: theological ",
  "loading, lexical drift in 17th-c. English (\"prevent\", \"conversation\", ",
  "\"bowels\"), manuscript variants, idiom compression, word-order changes ",
  "that shift emphasis, and proper-noun transliteration. It returns a tag ",
  "plus an 80-140-word concrete, falsifiable explanation that quotes the ",
  "Greek lemma when it matters. The model is OpenAI ",
  mono["gpt-5.5"], " with ",
  mono["reasoning_effort = high"], "."}],

hd1["4. Worked case study: Romans 8"],
para[{
  "We work the framework end-to-end on a single, theologically dense ",
  "chapter \[LongDash] ", bold["Romans 8"], ", 39 verses, the locus classicus ",
  "of Pauline pneumatology. A whole-Bible heatmap is impressive but ",
  "hard to read; a single chapter lets us look at the actual prose."}],

hd2["4.1 Pipeline applied per verse"],
para[{
  "For every verse in the chapter: (a) GPT-5.5 with ",
  mono["reasoning_effort = high"],
  " produces a careful modern English translation of the SBLGNT Greek ",
  "(it has not seen the KJV at this point); (b) we embed the KJV and ",
  "the modern translation, both English, with ",
  mono["text-embedding-3-large"], "; (c) cosine distance between the two ",
  "embeddings becomes our per-verse divergence score. Same-language ",
  "embedding sidesteps the cross-lingual weakness most multilingual ",
  "embedders exhibit \[LongDash] the signal is now purely semantic."}],
codeCell["LostInTranslation`Romans8DistChart[]"],
Sequence @@ imageCell["section_romans-8_distance.png",
  "Romans 8: per-verse cosine distance between the KJV and a fresh GPT-5.5 modern translation, both embedded with text-embedding-3-large. Peaks identify where the KJV has made a non-obvious translation choice."],

hd2["4.2 The top divergent verses, with GPT-5.5 nuance analysis"],
para[{
  "For each of the top-", bold[ToString @ Length[nuanceRows]],
  " divergent verses we hand GPT-5.5 the Greek, the KJV, its own ",
  "modern translation, and the cosine distance, and ask it to ",
  ital["articulate the nuance"], " \[LongDash] the small but real differences ",
  "a casual reader misses. We explicitly tell it to skip trivial things ",
  "(\"thou\" vs \"you\", \"begat\" vs \"fathered\") and focus on ",
  "theological loading, archaic English meaning-drift, manuscript ",
  "variants, idiom handling, and word-order or syntactic compression ",
  "that changes emphasis. Reasoning effort is again ",
  mono["high"], "."}],
codeCell["(* The Romans 8 verse exhibits below are frozen GPT-5.5 outputs, shipped\n   as $frozen[\"romans8nuance\"] inside lost_in_translation.wl. To rerun the\n   nuance pipeline live (\[TildeTilde] $1 in OpenAI calls): for each verse, call\n      LostInTranslation`Audit[greek, \"Koine Greek\", kjvCandidate,\n         Model -> \"gpt-5.5\"]\n   and read the result's \"nuance\" entry. *)"],
Sequence @@ If[nuanceRows === {},
  {Cell[TextData[{StyleBox["[Run wolframscript -file section_analysis.wls -- romans-8 to populate this section.]",
    Italic, FontColor -> GrayLevel[0.5]]}], "Text",
    CellMargins -> {{80, 80}, {6, 6}}]},
  Flatten[verseExhibit /@ nuanceRows, 1]],

hd2["4.3 Reading the categories"],
para[{
  "Across Romans 8, GPT-5.5's tag is dominated by ",
  mono["archaic-english-drift"], " \[LongDash] the KJV English of 1611 ",
  "carried senses that 21st-century readers no longer hear (\"creature\" ",
  "for ", ital["the created order"], ", \"carnal\" for ", ital["of the flesh"],
  ", \"freely\" for ", ital["gratuitously"], "). The interesting ",
  "second-order pattern is that the cosine distance picks these up ",
  "even though both renderings are formally accurate translations of ",
  "the same Greek. That is exactly the ",
  ital["nuance"], " signal: at 0.05 cosine the verses are saying the ",
  "same thing; at 0.20 they are still saying the same thing, but the ",
  "1611 English now ", ital["sounds different"], " to a modern ear, and ",
  "GPT-5.5 can articulate why."}],
hd2["4.4 The lexical drift atlas"],
para[{
  "A small distillation of the recurring \"English drift\" entries ",
  "from the Romans 8 nuance table: five KJV words whose 1611 sense is ",
  "no longer dominant. The table also names the Greek lemma at the ",
  "verse-anchoring position so a reader can jump straight to the ",
  "primary text. The same exercise on 1 Corinthians 13 (\[Section] 5) is ",
  "dominated by a single word: ", ital["charity"], "."}],
codeCell["(* Lexical-drift mini-table: hand-curated entries chosen from the Romans 8\n   nuance JSON (shipped as $frozen[\"romans8nuance\"]) and laid out with\n   Grid[]. Rendered once by wolfram/extra_visualisations.wls; the PNG is\n   embedded below. *)"],
Sequence @@ imageCell["lexical_drift_table.png",
  "Five Romans 8 KJV words whose 1611 / OED sense diverges from their modern dominant sense, with the Greek lemma the verse turns on."],

hd1["5. A second worked case: 1 Corinthians 13"],
para[{
  "The same pipeline on the \"love chapter\" \[LongDash] 13 verses, ",
  "famously rendered with \"charity\" in the KJV where every modern ",
  "translation has \"love\". This is the textbook example of ",
  bold["semantic narrowing"], ": ", ital["charity"], " in 1611 still ",
  "meant ", ital["self-giving love"], " (Greek ", "\[Alpha]\[Gamma]\[Alpha]\[Pi]\[Eta]", "); today it means ",
  ital["alms-giving to the poor"], ". The framework finds the chapter's ",
  "high-divergence verses automatically and GPT-5.5 articulates the ",
  "narrowing per-verse."}],
codeCell["LostInTranslation`Cor13DistChart[]"],
Sequence @@ imageCell["section_1-corinthians-13_distance.png",
  "1 Corinthians 13: per-verse cosine distance between the KJV and a fresh GPT-5.5 modern translation."],
codeCell["(* The verse exhibits below come from $frozen[\"cor13nuance\"]. To rerun\n   live: call LostInTranslation`Audit[greek, \"Koine Greek\", kjv,\n     Model -> \"gpt-5.5\"] per verse. *)"],
Sequence @@ If[cor13["nuance"] === {},
  {Cell[TextData[{StyleBox["[Run wolframscript -file section_analysis.wls -- 1-corinthians-13 8 to populate this section.]",
     Italic, FontColor -> GrayLevel[0.5]]}], "Text",
     CellMargins -> {{80, 80}, {6, 6}}]},
  Flatten[verseExhibit /@ cor13["nuance"], 1]],

hd1["6. Generalisation: Iliad and Daodejing"],
para[{
  "Two worked-out generalisations, on texts and languages far from the ",
  "Greek NT, that the same pipeline runs unchanged."}],

hd2["6.1 Iliad Book 1 \[LongDash] Greek + Pope (1715) + Butler (1898)"],
para[{
  "Three iconic passages of Homer rendered three ways: the Homeric ",
  "Greek, ", bold["Alexander Pope's"], " 1715-20 heroic-couplet ",
  "translation (the canonical English Iliad of the eighteenth century, ",
  "famous for its neoclassical compression and moral amplifications), ",
  "and ", bold["Samuel Butler's"], " 1898 prose translation (the ",
  "Victorian de-mythologising response). GPT-5.5 produces a fresh ",
  "modern English rendering of each Greek passage, and we compute the ",
  "pairwise within-English cosine distances. The framework now has ",
  ital["three"], " same-language English texts to compare, giving us ",
  "a four-corner picture per passage."}],
codeCell["LostInTranslation`IliadDistanceMatrix[]"],
Sequence @@ imageCell["iliad_distance_matrix.png",
  "Iliad Book 1: pairwise within-English cosine distances for three iconic passages. \
Pope's verse sits farther from both Butler's prose and a modern rendering than \
Butler and the modern do from each other \[Dash] the framework picks up Pope's \
heroic-couplet expansions automatically."],
codeCell["(* The three Iliad passages \[Dash] each rendered four ways (Homeric Greek +\n   Pope + Butler + a fresh GPT modern) \[Dash] are frozen in $frozen[\"iliad3way\"].\n   Per-passage nuance commentary follows. *)"],
Sequence @@ If[iliadRows === {},
  {Cell[TextData[{StyleBox["[Run wolframscript -file generalisation_iliad.wls to populate this section.]",
     Italic, FontColor -> GrayLevel[0.5]]}], "Text",
     CellMargins -> {{80, 80}, {6, 6}}]},
  Flatten[iliadExhibit /@ iliadRows, 1]],

hd2["6.2 Daodejing chs. 1-2 \[LongDash] Chinese + Legge (1891) + modern"],
para[{
  "The same translate-then-compare pipeline on a text that has nothing to ",
  "do with the Greek-vs-English Christian tradition. ",
  bold["James Legge"], "'s 1891 translation is the canonical Victorian ",
  "sinological Daodejing; GPT-5.5 produces a fresh modern English ",
  "rendering of the Wang Bi recension. The interesting nuances cluster ",
  "around abstract-noun choices (", ital["Dao"], ", ", ital["wu"], "/",
  ital["you"], ", ", ital["xuan"], "), Victorian parenthetical habits ",
  "(\"(the idea of)\"), and Legge's habit of importing Christian ",
  "conceptual vocabulary (\"Originator\", capitalised \"Mystery\"). The ",
  "framework finds them without being told."}],
codeCell["LostInTranslation`TaoDistanceChart[]"],
Sequence @@ imageCell["tao_distance.png",
  "Daodejing chs. 1-2: cross-language distances (Chinese vs each English rendering) \
sit around 0.85 \[Dash] essentially uncorrelated by language. The English-only \
Legge-vs-Modern distance is ~0.20-0.23, exactly the nuance band we want to read."],
codeCell["(* The Daodejing two-chapter audit \[Dash] Wang Bi Chinese vs Legge 1891 vs a\n   fresh GPT-5.5 English rendering \[Dash] is frozen in $frozen[\"tao2way\"]. *)"],
Sequence @@ If[taoRows === {},
  {Cell[TextData[{StyleBox["[Run wolframscript -file generalisation_tao.wls to populate this section.]",
     Italic, FontColor -> GrayLevel[0.5]]}], "Text",
     CellMargins -> {{80, 80}, {6, 6}}]},
  Flatten[taoExhibit /@ taoRows, 1]],

hd2["6.3 Other text pairs the framework runs on without modification"],
para[{
  "Because the framework is a thin layer over verse/sentence-aligned ",
  "JSON, anything you can fetch and align is fair game. Concrete next ",
  "candidates with public-domain sources: ", bold["KJV vs Luther 1545"],
  " on a single NT chapter (English vs German, where the bridge is a ",
  "fresh GPT-5.5 modern English rendering of the German); ", bold["Marx's ",
  ital["Kapital"], " vol. I"], " ch. 1, German vs Moore-Aveling 1887 ",
  "(the interesting nuances are around technical vocabulary that did ",
  "not yet exist in English in 1887: ", ital["Verdinglichung"], ", ",
  ital["Tauschwert"], ", ", ital["Wertform"], "); two Quran translations ",
  "(Sale 1734, Pickthall 1930) on a Surah; ", ital["Beowulf"],
  " in two PD modern renderings. Each of these would slot into ",
  mono["wolfram/section_analysis.wls"], "'s ", mono["sections"],
  " registry as a one-line addition."}],

hd1["7. Cross-section analysis"],
para[{
  "With four sections in hand we can ask cross-cutting questions \[Dash] ",
  "what kinds of drift dominate where, do the chapters separate cleanly ",
  "in embedding space, and how robust is the ranking to a change of ",
  "embedding model."}],

hd2["7.1 Nuance-category distribution across the four sections"],
para[{
  "Each section's top-N divergent units carry an LLM-tagged ",
  "category. Stacking those tags per section gives a rough \"signature\" ",
  "of how a translation diverges from its source. On Romans 8 and ",
  "1 Corinthians 13 the dominant tag is ", mono["archaic-english-drift"],
  ", consistent with the chapters being theologically straightforward ",
  "and the divergence being mainly lexical. The Iliad and Daodejing ",
  "schemas are different (we asked the model for ", mono["pope_signature"],
  " and ", mono["victorian_marker"], " etc.) so they appear as single ",
  "bins; the point is that the same pipeline produced both."}],
codeCell["(* Category-tag distribution: a BarChart over the LLM-chosen \"category\"\n   tags in $frozen[\"romans8nuance\"] and $frozen[\"cor13nuance\"]. Rendered\n   once by wolfram/extra_visualisations.wls; PNG embedded below. *)"],
Sequence @@ imageCell["category_distribution.png",
  "Distribution of LLM-tagged categories across the four worked sections. Romans 8 and 1 Cor 13 are dominated by archaic-English drift; the Iliad and Daodejing rows use schemas tailored to those texts."],

hd2["7.2 The English embedding space: a 2D PCA projection"],
para[{
  "Concatenate every KJV and every GPT-modern English rendering from ",
  bold["Romans 8"], " and ", bold["1 Corinthians 13"],
  ", project the 3072-dimensional embeddings onto their first two ",
  "principal components, and colour by side (KJV vs Modern) and section. ",
  "Two structural facts pop out: (a) chapters separate cleanly along ",
  "PCA dimension 1 \[Dash] Romans 8 is content-clustered on the right, ",
  "1 Cor 13 on the left; (b) within each chapter, KJV and Modern dots ",
  "are interleaved, confirming that same-language embedding distance is ",
  "the within-chapter signal we want."}],
codeCell["(* PrincipalComponents over every KJV + GPT-modern English embedding for\n   Romans 8 and 1 Corinthians 13, reading vectors from the embedding cache.\n   Rendered once by wolfram/extra_visualisations.wls; PNG embedded below. *)"],
Sequence @@ imageCell["pca_embeddings.png",
  "PCA of the English embeddings (text-embedding-3-large). Romans 8 (dark) clusters on the right; 1 Corinthians 13 (light) clusters on the left. KJV and Modern interleave within each chapter \[Dash] the picture an embedder ought to give us."],

hd2["7.3 Embedding-model dependency: large vs small"],
para[{
  "Distances are not portable across embedding models. We re-embed the ",
  "Romans 8 KJV/modern pairs with ", mono["text-embedding-3-small"],
  " (1536-d) and compare to ", mono["text-embedding-3-large"],
  " (3072-d). Aggregate agreement is high ",
  "(Pearson r = 0.880, Spearman \[Rho] = 0.898), but the per-verse ranking ",
  "moves: the most-divergent verse under the large model (Romans 8:21, ",
  ital["creature"], "/", ital["creation"], ") drops several ranks under ",
  "the small model, while Romans 8:1 (the Textus Receptus expansion) ",
  "rises into the small model's top-5. If you swap embedding models, ",
  "rerun the whole ranking."}],
codeCell["LostInTranslation`EmbeddingModelComparison[]"],
Sequence @@ imageCell["embedding_comparison_scatter.png",
  "Romans 8 cosine distances under text-embedding-3-large vs text-embedding-3-small. The dashed line is y=x. Verses above the line are judged more divergent by the small model than the large; verses below, the reverse. Aggregate correlation is high but the tail of the ranking is model-dependent."],

hd2["7.4 Translation-drift phylogeny across six PD English NTs"],
para[{
  "What happens if we apply the framework to two ", ital["English"],
  " translations, with no Greek in sight? Romans 8 in ", bold["Tyndale"],
  " 1534, ", bold["Geneva"], " 1599, ", bold["KJV"], " 1611, ",
  bold["ASV"], " 1901, ", bold["BBE"], " 1949, ", bold["WEB"], " 2000. ",
  "Embed each translation's chapter as a single vector with ",
  mono["text-embedding-3-large"],
  ", compute pairwise cosine distance, build the hierarchical-clustering ",
  "tree with ", mono["Dendrogram"], ". The result, in one chart, recovers ",
  "what a textual historian would draw by hand:"}],
codeCell["LostInTranslation`DriftDendrogram[]"],
Sequence @@ imageCell["drift_phylogeny.png",
  "Dendrogram from pairwise cosine distance between six PD English NT translations on Romans 8. Three families pop out: Tyndale/Geneva (Reformation-era English), KJV/ASV/WEB (the King James lineage and its modern public-domain revisions), and BBE on its own (the only one in deliberately simplified vocabulary)."],
para[{
  "Three families: Tyndale + Geneva (Reformation-era English) on the bottom ",
  "branch; the KJV lineage and its public-domain modern descendants ASV / ",
  "WEB in a tight cluster; and the BBE \[Dash] the only translation in ",
  "deliberately simplified vocabulary \[Dash] on its own branch. The KJV ",
  "1611 and ASV 1901 sit at cosine distance ", mono["0.016"],
  " from each other (the smallest non-self distance in the matrix) ",
  "because the ASV is literally a careful revision of the KJV. The framework ",
  "does not know any of this; it reads it directly from the verses."}],

hd2["7.5 Honest caveat: length vs divergence"],
para[{
  "Does the framework over-detect long verses? Plotting KJV character length ",
  "against cosine distance for every Romans 8 verse gives a small ",
  bold["negative"], " correlation (Spearman \[Rho] = -0.25). Longer verses ",
  "are slightly ", ital["more"],
  " similar between translations, not less. This makes sense: a longer verse ",
  "carries more content for the embedding to anchor on, and the per-character ",
  "share of any one stylistic choice is smaller. The reverse worry \[Dash] ",
  "that the framework is implicitly a length detector \[Dash] is not borne out."}],
codeCell["LostInTranslation`LengthVsDivergence[]"],
Sequence @@ imageCell["length_vs_divergence.png",
  "KJV character length vs cosine distance for every Romans 8 verse. Spearman \[Rho] = -0.25 (small negative). The four labelled outliers are the high-divergence verses already discussed in \[Section] 4. The dashed line is the linear fit."],

hd1["8. Discussion"],

hd2["8.1 What the cosine ranking actually surfaces"],
para[{
  "The framework's headline claim is modest: same-language cosine ",
  "distance between two renderings of the same source is a usable, ",
  "automatic ranking of how hard the renderings disagree. On Romans 8, ",
  "1 Corinthians 13, three passages of the Iliad, and two chapters of ",
  "the Daodejing, the ranking matches what a scholar would flag by hand: ",
  "Romans 8:1 (Textus Receptus expansion), Romans 8:21 (\"creature\" vs ",
  "\"creation\"), 1 Corinthians 13 \"charity\" throughout, Iliad's opening ",
  "invocation (Pope's amplification of \[Mu]\[Eta]\[Nu]\[Iota]\[Sigma]), Daodejing ch. 1 ",
  "(Legge's \"Originator\" for \[CapitalDelta]\[Alpha]\[Omicron] vs a plainer rendering). ",
  "The ranking is doing real work."}],

hd2["8.2 What the LLM nuance pass is good at, and what it is not"],
para[{
  bold["Good at"], ": naming the Greek (or Chinese, or German) lemma the ",
  "divergence hinges on; identifying when a KJV word has drifted in ",
  "English usage (\"prevent\", \"charity\", \"conversation\", \"creature\"); ",
  "spotting Textus Receptus expansions and pointing at the families of ",
  "witnesses (P46, Sinaiticus, Vaticanus) when the variant is well-known; ",
  "noticing Victorian parenthetical habits and Christianising vocabulary ",
  "in Legge; tracking Pope's neoclassical personifications."}],
para[{
  bold["Bad at"], ": precise page citations to printed editions (we tell ",
  "the model in the prompt not to invent citations, but it occasionally ",
  "tries); identifying ", ital["rare"], " textual variants not in ",
  "standard apparatus discussions; distinguishing genuine theological ",
  "loading from incidental word choice; and confidently calling a tag ",
  "for borderline cases. Every nuance entry should be read as ", ital["a ",
  "hypothesis worth checking in Metzger / BDAG / BDF"], ", not as a ",
  "finding. Categories like ", mono["theological-loading"], " and ",
  mono["translation-style"],
  " are particularly soft; ", mono["textual-variant"], " and ",
  mono["archaic-english-drift"], " are firmer."}],

hd2["8.3 Use for religious-studies, classics and history-of-translation researchers"],
para[{
  "The most useful product of this pipeline is the ", bold["ranked + ",
  "tagged table"], " in ", mono["data/section_*_nuance.json"],
  ". A scholar reading it can: (1) skim the top-N divergent verses in 60 ",
  "seconds; (2) cluster by the LLM's category tag to see what kind of ",
  "drift dominates a chapter (Romans 8 is overwhelmingly ",
  mono["archaic-english-drift"],
  "; 1 Cor 13 is dominated by the ", ital["charity"], " narrowing; the ",
  "Iliad headlines on poetic compression); (3) follow each cross-ref into ",
  "a commentary; (4) lift the per-verse JSON straight into a ",
  "courseware or paper as a paragraph-sized scholion. The framework ",
  "does not replace consulting Metzger, BDAG or Wallace \[LongDash] but it ",
  "tells you which verses to consult them for."}],

hd2["8.4 Use for translators considering revision"],
para[{
  "A translation committee revising an existing English text (e.g. an ",
  "ESV second edition, or a new edition of Butler) can run their draft ",
  "against the predecessor through this framework. Verses where the ",
  "cosine distance is large but the LLM tags it ", mono["other"], " or ",
  mono["translation-style"], " are exactly the places where the committee ",
  "is making real but unmotivated stylistic choices. Verses where the ",
  "distance is large and the LLM tags it ", mono["textual-variant"],
  " or ", mono["theological-loading"], " are the principled changes; ",
  "those should be ", ital["explicit"], " in a translator's preface, ",
  "not silent."}],

hd2["8.5 Limits, caveats, and how to abuse this responsibly"],
para[{
  bold["1. Embedding model dependency."],
  " Cosine distances are not comparable across embedding models, and ",
  "different models will rank verses differently in the tail. We pick ",
  "one model (", mono["text-embedding-3-large"],
  ") and stick with it across all sections of the post; if you swap ",
  "models, re-run everything."}],
para[{
  bold["2. LLM confabulation."],
  " GPT-5.5 occasionally cites manuscript witnesses (P46 \"in B and D\") ",
  "where the witness is not actually attesting the variant in question. ",
  "Treat all witness lists in the nuance text as plausibility claims to ",
  "verify against NA28's apparatus, not facts to quote. The prompt asks ",
  "the model to omit citations it is unsure of, but the instruction is ",
  "soft."}],
para[{
  bold["3. Selection effects."],
  " By design we look at the top-N divergent verses, which biases the ",
  "story toward chapters where the KJV (or Pope, or Legge) made an ",
  "interesting choice. The middle of the distribution is large and ",
  "boring; do not generalise from the tail."}],
para[{
  bold["4. Do not cite the LLM as authoritative."],
  " In a paper, the framework is a way to ", ital["find"],
  " interesting verses. The argument about each verse has to be made ",
  "from primary sources and standard reference works. Cite Metzger, BDAG, ",
  "the OED, or whatever you found ", ital["after"], " the LLM pointed ",
  "you at the verse, not the LLM itself."}],

hd2["8.6 Cost and reproducibility"],
para[{
  "Romans 8 + 1 Cor 13 + the Iliad and Daodejing generalisations cost ",
  "in the low single-digit US dollars in GPT-5.5 calls and pennies in ",
  "embeddings. Every step is deterministic up to OpenAI sampling: ",
  "intermediate JSON files are committed, the distance plots are ",
  "Wolfram-generated, and the gpt-image-2 illustrations are committed ",
  "as PNGs so a rebuild of the notebook does not call OpenAI again. ",
  "See \[Section] 11 for the exact reproducibility script."}],

hd1["9. An interactive translator-audit tool"],
para[{
  "Everything in \[Section] 3-7 can be packaged as a small interactive ",
  "tool a translator might actually use. Paste a source-language ",
  "passage and one to three candidate English renderings; the tool ",
  "produces its own independent modern-English baseline, embeds all ",
  "renderings with ", mono["text-embedding-3-large"], ", ranks the ",
  "candidates by cosine distance to the baseline, and asks GPT-5.5 ",
  "(", mono["reasoning_effort = high"], ") for a per-candidate nuance ",
  "report \[Dash] what the candidate does distinctively, which source ",
  "lemma it turns on, and what an alternative phrasing might be."}],

hd2["9.1 Recommended interface: LostInTranslation`Audit[\[Ellipsis]]"],
para[{
  "The fastest path to using the tool is the synchronous ",
  mono["Audit[]"], " function. It takes a source passage, a source-",
  "language name, one or more candidate translations, and an optional ",
  mono["Model -> "], "selector. It prints stage-by-stage progress to ",
  "the notebook as it runs and returns a formatted ", mono["Column[]"],
  " with the baseline, the ranked candidates, and the per-candidate ",
  "nuance commentary. Wall-clock: \[TildeTilde] 10\[Dash]30 s with ",
  mono["gpt-5.4"], " (default), \[TildeTilde] 60\[Dash]180 s with ",
  mono["gpt-5.5"], " (deeper reasoning)."}],
para[{
  "The example cell below is ready to evaluate as-is. The setup cell at the \
top of the notebook (", mono["Get[\"lost_in_translation.wl\"]"], ") has \
already loaded the package; just Shift-Enter. It runs an audit on the \
famous opening sentence of ", ital["Don Quixote"], " (Cervantes, 1605) \
against three English renderings: a 1612 Shelton-style archaic, a modern \
paraphrase, and a literal modern. Replace the strings with your own text \
to audit anything else."}],
Cell[StringJoin[
   "LostInTranslation`Audit[\n",
   "   \"En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no \"\n",
   "    <> \"ha mucho tiempo que viv\[IAcute]a un hidalgo de los de lanza en \"\n",
   "    <> \"astillero, adarga antigua, roc\[IAcute]n flaco y galgo corredor.\",\n",
   "   \"Spanish\",\n",
   "   \"In a certain place of La Mancha, the name whereof I have no mind \"\n",
   "    <> \"to call to remembrance, there lived not long since a gentleman \"\n",
   "    <> \"of those that keep a lance upon the rack, an ancient buckler, \"\n",
   "    <> \"a lean hack, and a coursing greyhound.\",\n",
   "   \"Somewhere in La Mancha, in a village whose name I prefer to leave \"\n",
   "    <> \"forgotten, there once lived a minor nobleman with the usual \"\n",
   "    <> \"signs of faded status: an old lance, a battered shield, a \"\n",
   "    <> \"scrawny horse, and a hunting dog.\",\n",
   "   \"In a place in La Mancha, whose name I do not wish to remember, \"\n",
   "    <> \"not long ago there lived a gentleman of the kind who had a \"\n",
   "    <> \"lance in the rack, an old shield, a skinny nag, and a \"\n",
   "    <> \"greyhound for coursing.\",\n",
   "   LostInTranslation`Model -> \"gpt-5.4\"   (* or \"gpt-5.5\" *)\n",
   "]"
   ],
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 8}}],
para[{"Wall-clock: \[TildeTilde] 15-25 s with the default ",
  mono["gpt-5.4"], "; \[TildeTilde] 90-180 s with ", mono["gpt-5.5"], "."}],

hd2["9.2 A frozen worked example"],
para[{
  "The output below was produced by one run of ", mono["Audit[]"],
  " on ", ital["1 Corinthians 13:1"], " (the \"tongues of men and of ",
  "angels\" verse) with three candidate renderings: KJV 1769; a ",
  "modern NIV-style rendering; and a deliberately paraphrastic ",
  "rendering for contrast. The frozen ", mono["Column[]"], " is what ",
  mono["Audit[]"], " returns; the kernel also prints stage-by-stage ",
  "progress as it runs."}],
codeCell["(* The frozen Audit[] result rendered below was produced by one call to\n      LostInTranslation`Audit[\n         greek1Cor13_1, \"Koine Greek\",\n         kjvRendering, modernRendering, paraphraseRendering,\n         Model -> \"gpt-5.5\"]\n   The ranked baseline + per-candidate nuance commentary is also shipped\n   inside the package as $frozen[\"translatorExample\"]. *)"],
Sequence @@ If[AssociationQ[toolExample],
   toolExampleCells[toolExample],
   {Cell[TextData[{StyleBox["[Run wolframscript -file run_tool_example.wls to populate this section.]",
      Italic, FontColor -> GrayLevel[0.5]]}], "Text",
      CellMargins -> {{80, 80}, {6, 6}}]}],

hd2["9.3 Choosing a model"],
para[{
  "The ", mono["Model -> "], "option accepts any chat- or reasoning-",
  "capable OpenAI model the configured key has access to. Two practical ",
  "defaults:"}],
para[{bold[mono["\"gpt-5.4\""]],
  " (default) \[LongDash] a standard chat model. No reasoning phase: ",
  "the first response token appears within a second. Translation and ",
  "nuance commentary complete in 2\[Dash]4 s per call. This is what ",
  "makes the audit fast enough for interactive use."}],
para[{bold[mono["\"gpt-5.5\""]],
  " \[LongDash] a reasoning model. Spends 15\[Dash]60 s of internal ",
  "\"thinking\" before any visible output, but produces slightly more ",
  "thorough commentary on hard passages. Use when the difference is ",
  "worth the wait \[Dash] e.g. a single tricky verse you intend to ",
  "publish on. The corpus pipeline in \[Section] 4\[Dash]5 already ",
  "runs on ", mono["gpt-5.5"], " with high reasoning effort, offline."}],
para[{
  "The live list of every model your key can call \[Dash] including ",
  "newer chat or reasoning variants OpenAI may have shipped since this ",
  "post \[Dash] is one function call:"}],
Cell["LostInTranslation`AvailableOpenAIModels[]",
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 8}}],
para[{"Returns a sorted list filtered to chat and reasoning families ",
  "(skips image / audio / embedding endpoints). Cached for the rest of ",
  "the kernel session; pass ", mono["Refresh -> True"], " to re-fetch."}],

hd2["9.4 Optional GUI: TranslationAuditTool[]"],
para[{
  "If you prefer a panel with input fields and a button, ",
  mono["LostInTranslation`TranslationAuditTool[]"],
  " returns a ", mono["DynamicModule"], " that runs the audit ",
  bold["asynchronously on a fresh sub-kernel"], " via ",
  mono["LocalSubmit"], ". The FrontEnd-attached main kernel never ",
  "blocks; the panel stays responsive throughout. A model popup at ",
  "the top of the panel lets you pick any model from ",
  mono["AvailableOpenAIModels[]"], "."}],
para[{
  "Once ", mono["Get[\"lost_in_translation.wl\"]"], " has been evaluated:"}],
Cell["LostInTranslation`TranslationAuditTool[]",
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 8}}],
para[{
  "Fill in the source and one to three candidates, choose the model, ",
  "click ", bold["Audit"],
  ". A spinner appears next to the button; the sub-kernel does the ",
  "work; when it finishes the panel renders the ranked candidates and ",
  "their per-candidate nuance. The sub-kernel is fresh on every click, ",
  "so no state leaks between audits."}],

hd2["9.5 Implementation notes"],
para[{
  "The consolidated companion package, ", mono["lost_in_translation.wl"],
  " (HTTP transport, embedding cache, prompt templates, pipeline, UI, all \
chart functions and frozen data \[Dash] under 1 200 lines + ~160 KB of frozen \
JSON), drops in beside this notebook. Three implementation details are worth \
flagging if you adapt this to your own text-pair work:"}],
para[{bold["1. HTTP transport is curl via RunProcess, not URLRead."],
  " Mathematica's bundled libcurl pool can wedge on a stale keep-alive ",
  "socket after a few sequential API calls inside the same kernel; the ",
  "next call then blocks forever in ", mono["send()"],
  " and neither ", mono["TimeConstraint"], " nor ", mono["TimeConstrained"],
  " can interrupt it cleanly. Shelling out to the system ", mono["curl"],
  " binary gives every request a fresh TCP/TLS socket and a real OS-",
  "level ", mono["--max-time"], " timeout."}],
para[{bold["2. The interactive panel runs on a sub-kernel."],
  " Even with curl, doing a sequence of HTTP calls synchronously from ",
  "inside a ", mono["Button"], " body can starve the FrontEnd's ",
  mono["Dynamic"], " update channel and make the panel look frozen. ",
  mono["LocalSubmit"], " spawns a fresh Wolfram sub-kernel that does ",
  "the work; the main kernel polls the resulting ", mono["TaskObject"],
  " for completion. The pattern is the only one that reliably keeps ",
  "the UI responsive across kernel sessions."}],
para[{bold["3. Wolfram's built-in LLM functions are a viable alternative."],
  " ", mono["LLMSynthesize"], ", ", mono["LLMFunction"], " and ",
  mono["LLMConfiguration"], " offer a service-agnostic abstraction over ",
  "the major LLM providers, e.g.:"}],
Cell["LLMSynthesize[\n   {LLMPrompt[\"Translate this Koine Greek verse \" <>\n      \"into a careful modern English rendering. Reply with the \" <>\n      \"translation only.\"], source},\n   LLMConfiguration[<|\n      \"Service\"          -> \"OpenAI\",\n      \"Model\"            -> \"gpt-5.4\"|>]]",
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 8}}],
para[{"We kept the HTTP route because it gives us explicit control over ",
  "the request body (notably ", mono["max_completion_tokens"],
  " and ", mono["reasoning_effort"], "), explicit control over the ",
  "transport (curl-via-RunProcess vs WL libcurl), and direct access to ",
  "OpenAI's full model list. A production deployment that doesn't need ",
  "any of that would happily use ", mono["LLMSynthesize"], "."}],

hd2["9.6 Uses"],
para[{bold["For translation committees"], " auditing a draft: run each ",
  "candidate rendering through the tool against a fresh baseline and ",
  "the predecessor edition. Distance + nuance commentary surfaces ",
  "exactly where the new draft is silently making a substantive choice."}],
para[{bold["For students of philology"], ": paste a verse and a ",
  "historical rendering; the tool will name the lemma, flag the ",
  "archaic-English drift, and suggest an alternative \[Dash] a very ",
  "fast feedback loop on close-reading exercises."}],
para[{bold["For comparative-literature classrooms"], ": run a poem or ",
  "passage through several PD English translations to give students a ",
  "scored, ranked list of how different the renderings actually are, ",
  "with the LLM articulating why."}],

hd1["10. The framework as a fact-checker: three multilingual cases"],
para[{
  "All the case studies so far have been ", ital["literary"],
  " \[Dash] Scripture, classical epic, Daoist philosophy. The same ",
  "pipeline applies, unchanged, when the source is a multilingual ",
  "human-rights treaty, a Vatican encyclical, or a UN Charter article. ",
  "The reader can run the same ", mono["Audit[\[Ellipsis]]"],
  " function from \[Section] 9 on any one-paragraph source, with any ",
  "number of candidate English renderings, and the framework will tell ",
  "them: which rendering sits closest to a fresh independent translation ",
  "of the source, where each candidate makes a distinctive choice, and ",
  "whether that distinctive choice is a faithful nuance or a framing ",
  "shift. The three cases below were all run by the same code as the ",
  "scripture audit; only the input data changed."}],
codeCell["(* The three cases below were produced by running\n      LostInTranslation`Audit[source, sourceLanguage, candidate1, candidate2,\n         Model -> \"gpt-5.5\"]\n   on UDHR Article 1 (French source), Laudato Si' \\\\sec 1 (Latin source) and\n   UN Charter Article 1 (Spanish source), each against two English\n   candidates. The frozen ranked+commentary table is\n   $frozen[\"newsFactChecker\"] inside the package. *)"],
Sequence @@ If[nfcCases === {},
   {Cell[TextData[{StyleBox["[Run wolframscript -file news_fact_checker.wls to populate this section.]",
      Italic, FontColor -> GrayLevel[0.5]]}], "Text",
      CellMargins -> {{80, 80}, {6, 6}}]},
   Flatten[newsCaseExhibit /@ nfcCases, 1]],
para[{bold["Reading the three together."],
  " The pattern that emerges is mechanical and useful. ",
  "On every passage, the rendering closest to the fresh independent ",
  "baseline is also the one a careful translator would pick first. ",
  "The framing shifts \[Dash] \"environmental encyclical\" for ",
  ital["Laudato Si'"], "; \"plain-language\" or \"headline\" rewrites ",
  "of legal text \[Dash] consistently sit farther away, exactly where ",
  "a fact-checker should look. None of this is hard to read off the ",
  "audit table: the distance numbers tell you ", ital["where"],
  " to look, and the GPT-5.4 commentary tells you ", ital["why"], "."}],

hd1["11. References & acknowledgements"],
para[{bold["SBLGNT"], " \[LongDash] Michael W. Holmes (ed.), ",
       ital["The Greek New Testament: SBL Edition"],
       " (SBL & Logos, 2010), CC BY 4.0. Source verses fetched ",
       "from the ", mono["morphgnt/sblgnt"], " repository."}],
para[{bold["MorphGNT"], " \[LongDash] J. K. Tauber (ed.), v6.12, CC BY-SA 3.0."}],
para[{bold["KJV"], " \[LongDash] Cambridge 1769 edition, public domain (US). ",
       "JSON via the ", mono["aruljohn/Bible-kjv"], " repository."}],
para[{bold["Iliad (Pope, 1715-20)"],
       " \[LongDash] Alexander Pope's heroic-couplet translation; ",
       "Project Gutenberg etext #6130, public domain."}],
para[{bold["Iliad (Butler, 1898)"],
       " \[LongDash] Samuel Butler's prose translation; ",
       "Project Gutenberg etext #2199, public domain."}],
para[{bold["Iliad (Greek)"],
       " \[LongDash] the standard Homeric Greek text, verified ",
       "against any modern Oxford / Teubner edition."}],
para[{bold["Daodejing (Wang Bi recension)"],
       " \[LongDash] Project Gutenberg etext #7337, public domain."}],
para[{bold["Daodejing (Legge, 1891)"],
       " \[LongDash] James Legge, ", ital["The Tao Teh King"],
       "; Project Gutenberg etext #216, public domain."}],
para[{bold["Embedding model"], " \[LongDash] OpenAI ",
       mono["text-embedding-3-large"],
       " (Cohere ", mono["embed-multilingual-v3.0"],
       " and Voyage ", mono["voyage-3"], " are also wired in ",
       mono["wolfram/framework.wl"], " as drop-in alternatives)."}],
para[{bold["Image-generation model"], " \[LongDash] OpenAI ",
       mono["gpt-image-2"],
       " for the three conceptual illustrations in \[Section] 3, ",
       "called from pure Wolfram (no Python) via the helper in ",
       mono["wolfram/generate_illustrations.wls"], "."}],
para[{bold["LLMs"],
       " \[LongDash] OpenAI ", mono["gpt-5.5"],
       " (with ", mono["reasoning_effort = high"], ") for the corpus-batch ",
       "nuance pipeline (\[Section] 4-7); OpenAI ", mono["gpt-5.4"],
       " (no reasoning phase) as the default for the interactive ",
       "translator-audit tool (\[Section] 9), with ", mono["gpt-5.5"],
       " selectable per-call via the ", mono["Model -> "], "option. ",
       "Both reached via ", mono["SystemCredential[\"OPENAI_API_KEY\"]"],
       ". Claude Opus 4.7 is wired as a drop-in alternative."}],

hd1["12. Reproducibility"],
para[{bold["Source repository"], " \[LongDash] ",
  Cell[BoxData @ ToBoxes @ Hyperlink[
    "https://github.com/mthiel74/GreekNewTestamentvsKingJames",
    "https://github.com/mthiel74/GreekNewTestamentvsKingJames"]],
  ". The repo carries every ", mono[".wls"],
  " pipeline script, the unconsolidated ", mono["framework.wl"], " / ",
  mono["translator_tool.wl"],
  ", all committed frozen JSON outputs, and the gpt-image-2 PNGs."}],
para[{"Full pipeline (\[TildeTilde] $2-3 of GPT-5.5 calls + cents of embedding + cents of gpt-image-2):"}],
Cell["wolframscript -file wolfram/fetch_kjv.wls\nwolframscript -file wolfram/fetch_greek_nt.wls\nwolframscript -file wolfram/fetch_historical_bibles.wls\nwolframscript -file wolfram/section_analysis.wls -- romans-8 10\nwolframscript -file wolfram/section_analysis.wls -- 1-corinthians-13 8\nwolframscript -file wolfram/generalisation_iliad.wls\nwolframscript -file wolfram/generalisation_tao.wls\nwolframscript -file wolfram/embedding_comparison.wls\nwolframscript -file wolfram/extra_visualisations.wls\nwolframscript -file wolfram/translation_drift.wls\nwolframscript -file wolfram/length_vs_divergence.wls\nwolframscript -file wolfram/news_fact_checker.wls\nwolframscript -file wolfram/generate_illustrations.wls\nwolframscript -file wolfram/run_tool_example.wls\nwolframscript -file community/build_notebook.wls",
  "Input", FontFamily -> "Courier",
  CellMargins -> {{60, 60}, {6, 16}}]
};

(* ====================================================================
   Render notebook
   ==================================================================== *)

Print["=== building ", outNb, " ==="];
UsingFrontEnd[
   nb = CreateDocument[{}, Visible -> False,
      WindowTitle -> "Translation-Pair Framework"];
   Do[NotebookWrite[nb, c], {c, cells}];
   NotebookSave[nb, outNb];
   (* Optional PDF export \[LongDash] guard against headless FE limitations. *)
   Quiet @ Check[Export[outPdf, nb], Print["  PDF export skipped (no FE)"]];
   NotebookClose[nb];
];
Print["  wrote ", outNb,
      " (", FileByteCount[outNb], " bytes)"];
If[FileExistsQ[outPdf],
  Print["  wrote ", outPdf,
        " (", FileByteCount[outPdf], " bytes)"]];
