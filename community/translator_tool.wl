(* ::Package:: *)

(*  TranslatorTool` \[Dash] interactive translation-audit tool.

    A small DynamicModule that lets a translator paste a source-language
    passage and one or more candidate English translations, then runs:

      1. GPT-5.4 produces an independent modern-English baseline
         rendering of the source (fast, ~2-3 s, no reasoning overhead).
         This is what the candidates are compared against.
      2. All renderings (baseline + candidates) are embedded with
         text-embedding-3-large. Cosine distance from each candidate
         to the baseline becomes the divergence score.
      3. GPT-5.4 produces a per-candidate nuance commentary:
         where the candidate makes a distinctive choice, what the
         source lemma is, and what an alternative rendering might be.

    Designed to live inside the Wolfram Community notebook. The PDF
    export captures the frozen initial state of the DynamicModule;
    in Mathematica the interface is live.
*)

BeginPackage["TranslatorTool`",
   {"TPFramework`"}];

TranslationAuditTool::usage =
   "TranslationAuditTool[] returns a DynamicModule \[Dash] paste source and \
candidates, click Audit, read the nuance report.";

RunAudit::usage =
   "RunAudit[source, sourceLang, candidates] returns an Association \
with baseline, distances, and nuance commentary.";

RunAuditProgressive::usage =
   "RunAuditProgressive[source, sourceLang, candidates, statusFn] is \
RunAudit with a per-stage status callback for live UI feedback.";

Audit::usage =
   "Audit[source, sourceLang, candidate1, candidate2, ..., Model -> name] \
returns a formatted Column[] with the baseline, ranked candidates, and \
nuance commentary. Prints progress at the kernel as it runs. \
Default model is gpt-5.4; pass Model -> \"gpt-5.5\" for a deeper but \
slower reasoning pass.";

Model::usage =
   "Model is an option for Audit, RunAudit, and RunAuditProgressive that \
selects which OpenAI chat model produces the baseline translation and \
per-candidate nuance commentary. Default \"gpt-5.4\". Pass any model \
returned by TPFramework`AvailableOpenAIModels[].";

Begin["`Private`"];

Needs["TPFramework`"];

(* --- per-language translate-prompt seasonings --- *)
seasoning[lang_] := Switch[lang,
   "Koine Greek",
      "Source is Koine Greek (1st-c. CE). Preserve aorist vs. perfect, \
middle vs. passive, and emphatic word order. Keep \[Sigma]\[Alpha]\[Rho]\[Xi] as \
\"flesh\" when Pauline-technical; \[Pi]\[Nu]\[CurlyEpsilon]\[Upsilon]\[Mu]\[Alpha] capitalised when \
divine; \[Beta]\[Alpha]\[Pi]\[Tau]\[Iota]\[Zeta]\[Omega] as \"baptise\" only when technical-religious.",
   "Classical Greek",
      "Source is Classical Greek (5th-4th c. BCE).",
   "Hebrew",
      "Source is Biblical Hebrew. Preserve verbal-stem distinctions \
(qal, niphal, piel, hiphil) where they matter semantically.",
   "Latin",
      "Source is Classical or post-Classical Latin. Preserve case and \
voice distinctions; do not flatten ablatives.",
   "Classical Chinese",
      "Source is Classical Chinese. Leave " <> FromCharacterCode[36947] <>
      " (Dao) as \"the Dao\"; " <> FromCharacterCode[24503] <>
      " (De) as \"virtue\" when ethical, \"inherent power\" when \
ontological; preserve parallelism.",
   _, ""];

(* --- the workhorse function (non-progressive form, used for the
   frozen example script) ---

   Audit, RunAudit and RunAuditProgressive all accept a Model option
   that selects which OpenAI chat model produces the baseline
   translation and per-candidate nuance commentary. Default gpt-5.4
   (fast, no reasoning); pass Model -> "gpt-5.5" for deeper analysis
   at the cost of 15-60 s of reasoning per call. *)
Options[RunAudit]            = {Model -> "gpt-5.4"};
Options[RunAuditProgressive] = {Model -> "gpt-5.4"};

RunAudit[source_String, sourceLang_String, candidates_List,
         opts:OptionsPattern[]] :=
   RunAuditProgressive[source, sourceLang, candidates,
      (* status callback ignored *) Function[s, Null],
      opts];

(* --- a progressive form that calls a status callback between
   stages. The DynamicModule below sets a Dynamic-bound variable in
   that callback so the UI shows live progress. --- *)
RunAuditProgressive[source_String, sourceLang_String, candidates_List,
                    statusFn_, OptionsPattern[]] := Module[
   {key, baseline, candList, embs, baseEmb, candEmbs,
    distances, ranked, commentary, model = OptionValue[Model]},
   key = OpenAIKey[];
   If[!StringQ[key] || key === "",
     Return[<|"error" -> "No OpenAI key in SystemCredential."|>]];

   candList = Select[StringTrim /@ candidates, StringLength[#] > 0 &];
   If[candList === {},
     Return[<|"error" -> "Provide at least one candidate translation."|>]];

   statusFn["1/3 " <> model <>
            " producing independent baseline translation \[Ellipsis]"];
   baseline = LLMTranslate[
      "(" <> sourceLang <> ") " <> source <> ".\n\n" <> seasoning[sourceLang],
      "", model];
   If[baseline === $Failed,
     Return[<|"error" -> "Baseline translation failed."|>]];

   statusFn["2/3 embedding baseline + " <>
      ToString[Length[candList]] <> " candidate(s) \[Ellipsis]"];
   embs = EmbedTexts[Prepend[candList, baseline],
      EmbeddingProvider -> "openai"];
   If[embs === $Failed,
     Return[<|"error" -> "Embedding failed."|>]];
   baseEmb  = First[embs];
   candEmbs = Rest[embs];
   distances = MapThread[CosineDistance[baseEmb, #1] &, {candEmbs}];

   ranked = SortBy[
      MapIndexed[
         <|"index" -> First[#2], "candidate" -> #1,
           "distance" -> distances[[First[#2]]]|> &,
         candList],
      #["distance"] &];

   commentary = MapIndexed[
      Function[{row, idx},
        statusFn["3/3 " <> model <> " nuance commentary for candidate " <>
           ToString[First[idx]] <> "/" <> ToString[Length[ranked]] <>
           " \[Ellipsis]"];
        runNuance[source, sourceLang, baseline,
                  row["candidate"], row["distance"], model]],
      ranked];

   statusFn[""];

   <|"source"     -> source,
     "sourceLang" -> sourceLang,
     "model"      -> model,
     "baseline"   -> baseline,
     "candidates" -> ranked,
     "commentary" -> commentary|>
];

runNuance[source_, sourceLang_, baseline_, candidate_, distance_,
          model_String:"gpt-5.4"] :=
 Module[{system, prompt, raw, parsed, key = OpenAIKey[]},
   system = "You audit one English translation of a single passage. \
You are given: the source-language passage, an independent modern \
English baseline rendering, and the candidate to be audited. Produce a \
brief, scholarly assessment: where does the candidate diverge from the \
baseline (and presumably from the source)? Quote the source-language \
lemma where it matters. Suggest at most one alternative rendering. \
Reply with a JSON object only (no markdown fences), with fields: \
\"distinctive_choice\", \"key_lemma\", \"suggested_alt\", \
\"explanation\" (60-110 words).";
   prompt = StringJoin[
      "Source (", sourceLang, "): ", source, "\n",
      "Baseline (independent modern English): ", baseline, "\n",
      "Candidate to audit: ", candidate, "\n",
      "Cosine distance candidate-vs-baseline: ",
      ToString @ NumberForm[distance, {4, 3}], "\n\n",
      "Reply with exactly:\n",
      "{\n",
      "  \"distinctive_choice\": \"<one-line summary>\",\n",
      "  \"key_lemma\": \"<source-language word, or empty>\",\n",
      "  \"suggested_alt\": \"<at most one alternative phrase, or empty>\",\n",
      "  \"explanation\": \"<60-110 words>\"\n",
      "}"];
   (* Caller picks the model. Default gpt-5.4 (fast, no reasoning).
      Passing gpt-5.5 trades 15-60 s of reasoning per call for slightly
      more thorough commentary. openaiCall strips reasoning_effort for
      non-reasoning models automatically. *)
   raw = TPFramework`Private`openaiCall[prompt, model, 4000, key,
      "medium", system, False];
   If[raw === $Failed, Return[<|"error" -> "nuance call failed"|>]];
   TPFramework`Private`parseJSONLoose[raw]
];

(* --- the no-GUI version (most reliable; recommended) --- *)

Options[Audit] = {Model -> "gpt-5.4"};

Audit[source_String, sourceLang_String, candidates___String,
      opts:OptionsPattern[]] := Module[
   {raw, model = OptionValue[Model]},
   Print[Style[
      "Running translation audit (" <> model <>
      ") \[Dash] this typically takes 10-30 s with gpt-5.4, 60-180 s with gpt-5.5.",
      FontColor -> RGBColor[0.3, 0.3, 0.4]]];
   raw = RunAuditProgressive[source, sourceLang, {candidates},
      Function[msg, Print[Style[msg, FontColor -> GrayLevel[0.45]]]],
      Model -> model];
   If[KeyExistsQ[raw, "error"],
     Return[Style["Error: " <> raw["error"], FontColor -> Red]]];
   Print[Style["done.", FontColor -> RGBColor[0.2, 0.5, 0.2]]];
   renderAuditResult[raw]
];

(* --- the DynamicModule (interactive; ASYNC via LocalSubmit) ---

   The Audit button submits the work to a fresh local sub-kernel via
   LocalSubmit[]. The FrontEnd-attached main kernel only handles UI
   redraws; it never blocks on HTTP. The main kernel polls the
   TaskObject every second via Dynamic[]; when the subkernel finishes
   the HandlerFunctions assign `result`, the Dynamic[] re-renders, and
   the panel displays the formatted output.

   This bypasses the entire class of bugs we hit before:
     - the main kernel's libcurl pool getting wedged after multiple
       sequential calls;
     - Mathematica's wait4() blocking on a Run-spawned shell wrapper
       after the first call in a kernel session;
     - synchronous Button bodies starving the FrontEnd's Dynamic
       update channel.

   The subkernel is fresh on each click: fresh process table, fresh
   libcurl pool, fresh /tmp namespace. *)

TranslationAuditTool[] := DynamicModule[
   {source = "", c1 = "", c2 = "", c3 = "",
    sourceLang = "Koine Greek",
    model = "gpt-5.4", availableModels,
    pkgDir, task = None, result = None, status = "ready"},
   (* Init: figure out the package directory we should Get from in
      the sub-kernel, and pull the live list of available chat
      models from the OpenAI /v1/models endpoint. *)
   pkgDir = With[{nbDir = Quiet @ NotebookDirectory[]},
      If[StringQ[nbDir], nbDir,
         "/Users/thiel/GitHub/GreekNewTestamentvsKingJames/community/"]];
   availableModels = Quiet @ AvailableOpenAIModels[];
   If[!ListQ[availableModels] || availableModels === {},
      availableModels = {"gpt-5.4", "gpt-5.5"}];
   Panel[Column[{
      Style["Translation Audit Tool", Bold, 18,
         FontColor -> RGBColor[0.5, 0.1, 0.1]],
      Style[
         "Paste a source-language passage and one to three candidate \
English translations. Click Audit; the panel stays responsive while a \
fresh sub-kernel does the work (~10-30 s with gpt-5.4, ~60-180 s with \
gpt-5.5). The output appears in this panel when the sub-kernel \
finishes.",
         FontSize -> 11, FontColor -> GrayLevel[0.45]],
      Row[{Style["Source language: ", Bold, 12],
           PopupMenu[Dynamic[sourceLang],
              {"Koine Greek", "Classical Greek", "Hebrew", "Latin",
               "Classical Chinese", "German", "Spanish"}],
           Spacer[20],
           Style["Model: ", Bold, 12],
           PopupMenu[Dynamic[model], availableModels]}],
      Style["Source text", Bold, 12],
      InputField[Dynamic[source], String, FieldSize -> {65, 3},
         BaseStyle -> {FontFamily -> "Times", FontSize -> 13}],
      Style["Candidate translation 1", Bold, 12],
      InputField[Dynamic[c1], String, FieldSize -> {65, 2}],
      Style["Candidate translation 2 (optional)", Bold, 12],
      InputField[Dynamic[c2], String, FieldSize -> {65, 2}],
      Style["Candidate translation 3 (optional)", Bold, 12],
      InputField[Dynamic[c3], String, FieldSize -> {65, 2}],
      Row[{
         (* The With[...] freezes the current values of the
            DynamicModule locals INTO the body that LocalSubmit
            holds. Without it the subkernel sees only the
            renamed symbol names, which it doesn't know. *)
         Button["Audit",
            With[{src = source, lg = sourceLang,
                  cs  = {c1, c2, c3}, mdl = model, dir = pkgDir},
              result = None;
              status = "submitting to fresh sub-kernel \[Ellipsis]";
              task = LocalSubmit[
                Get[dir <> "framework.wl"];
                Get[dir <> "translator_tool.wl"];
                TranslatorTool`RunAuditProgressive[src, lg, cs,
                   Function[m, Null], Model -> mdl],
                HandlerFunctions -> <|
                   "TaskFinished" -> Function[t,
                     result = t["EvaluationResult"];
                     status = "done."]|>]];
            status = "running in sub-kernel \[Ellipsis]",
            ImageSize -> 100, BaseStyle -> {FontWeight -> Bold}],
         Spacer[20],
         (* Live status: spinner while task is running, blank when
            idle, summary message otherwise. *)
         Dynamic[Refresh[
            Which[
              task === None,
                "",
              MatchQ[Quiet @ TaskStatus[task], "Pending" | "Running"],
                Row[{ProgressIndicator[Appearance -> "Necklace"],
                     Spacer[10],
                     Style[status, Italic, FontColor -> GrayLevel[0.4]]}],
              True,
                Style[status, Italic, FontColor -> GrayLevel[0.4]]],
            UpdateInterval -> 1, TrackedSymbols :> {task, status}]]
      }],
      Style["", FontSize -> 6],
      Dynamic[
         Which[
            result === None,
               Style["Output appears here once the sub-kernel finishes.",
                     Italic, GrayLevel[0.5]],
            AssociationQ[result] && KeyExistsQ[result, "error"],
               Style["Error: " <> result["error"],
                     FontColor -> RGBColor[0.6, 0, 0]],
            AssociationQ[result],
               renderAuditResult[result],
            True,
               Style["Unexpected result: " <> ToString[result],
                     FontColor -> GrayLevel[0.5]]]]
   }, Spacings -> 1.4],
   FrameMargins -> 16,
   Background -> RGBColor[0.99, 0.99, 0.96]]
];

renderAuditResult[res_Association] /; KeyExistsQ[res, "error"] :=
   Style["Error: " <> res["error"], FontColor -> RGBColor[0.6, 0, 0]];

renderAuditResult[res_Association] := Column[{
   Style["Baseline (GPT-5.5 modern):", Bold, 13],
   Style[res["baseline"], FontFamily -> "Helvetica", FontSize -> 12],
   Style["", FontSize -> 4],
   Style["Ranked candidates (closer to baseline first):", Bold, 13],
   Grid[
     Prepend[
       MapThread[
         {Style[ToString[#1["index"]], Bold],
          Style[#1["candidate"], FontFamily -> "Times", FontSlant -> Italic],
          Style["d = " <> ToString @ NumberForm[#1["distance"], {4, 3}],
             FontFamily -> "Courier", FontSize -> 11]} &,
         {res["candidates"], Range[Length[res["candidates"]]]}],
       {Style["#", Bold], Style["Candidate", Bold], Style["Distance", Bold]}],
     Frame -> All, FrameStyle -> GrayLevel[0.7], Alignment -> Left,
     Spacings -> {1.5, 1}],
   Style["", FontSize -> 4],
   Style["Per-candidate nuance:", Bold, 13],
   Sequence @@ MapIndexed[
      Function[{c, idx},
        With[{n = res["commentary"][[First[idx]]]},
          Panel[Column[{
             Style["Candidate " <> ToString[c["index"]] <>
                 "  (d = " <> ToString @ NumberForm[c["distance"], {4,3}] <> ")",
                Bold, 12, FontColor -> GrayLevel[0.3]],
             If[KeyExistsQ[n, "distinctive_choice"] && StringQ[n["distinctive_choice"]],
                Row[{Style["distinctive choice: ", Bold, 11],
                     n["distinctive_choice"]}], Nothing],
             If[KeyExistsQ[n, "key_lemma"] && StringQ[n["key_lemma"]] && n["key_lemma"] =!= "",
                Row[{Style["key lemma: ", Bold, 11],
                     Style[n["key_lemma"], FontFamily -> "Times", FontSlant -> Italic,
                        FontColor -> RGBColor[0.10, 0.20, 0.50]]}], Nothing],
             If[KeyExistsQ[n, "suggested_alt"] && StringQ[n["suggested_alt"]] && n["suggested_alt"] =!= "",
                Row[{Style["suggested alt: ", Bold, 11],
                     Style[n["suggested_alt"], FontSlant -> Italic]}], Nothing],
             If[KeyExistsQ[n, "explanation"] && StringQ[n["explanation"]],
                Style[n["explanation"], FontSize -> 11, FontColor -> GrayLevel[0.25]],
                Nothing]
          }, Spacings -> 0.8],
          FrameMargins -> 8, Background -> RGBColor[0.97, 0.97, 0.93]]]],
      res["candidates"]]
}, Spacings -> 1.2];

End[];
EndPackage[];
