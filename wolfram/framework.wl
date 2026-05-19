(* ::Package:: *)

(* ====================================================================
   TPFramework`  —  Translation-Pair Divergence Framework
   ====================================================================

   A single Wolfram-Language package that handles the full pipeline for
   any aligned text pair:

     LoadPair[name]              read two verse-keyed JSON files into
                                 a single aligned Dataset (inner-join
                                 on ref) plus the asymmetric sets
     EmbedTexts[strings, opts]   call a multilingual embedding API
                                 (Cohere / OpenAI / Voyage); on-disk
                                 cache keyed by hash + model
     CosineDistance[a, b]        canonical cosine distance
     RankDivergence[ds, embA, embB] rank aligned units by distance
     LLMExplain[row, opts]       ask the LLM to classify the gap and
                                 write a one-paragraph hypothesis

   Conventions

     * Embeddings are returned as a Dataset with columns
       {"text", "vector"} and cached in data/embeddings/<model>.mx
       keyed by SHA-256 of the input string.
     * Cosine distance is in [0, 2], but typically <0.5 for the same
       source verse rendered into different languages.
     * Distances are not directly comparable across embedding models
       — pick one and stick with it across the corpus.

   API keys are read from environment variables at call time. See
   docs/api_keys.md.
*)

BeginPackage["TPFramework`"];

LoadPair::usage           = "LoadPair[pairName, sideA_json, sideB_json] -> <|aligned, onlyA, onlyB|>.";
ListPairs::usage          = "ListPairs[] -> list of preconfigured pair names.";

EmbedTexts::usage         = "EmbedTexts[strings, opts] returns a list of numeric vectors. Uses on-disk cache.";
EmbeddingProvider::usage  = "Option of EmbedTexts. \"cohere\" (default) | \"openai\" | \"voyage\".";
EmbeddingModel::usage     = "Option of EmbedTexts. Defaults to provider's best multilingual model.";

(* The built-in System`CosineDistance does exactly what we want
   (1 - a.b / (|a||b|)); we expose it from our package as
   TPFramework`CosineDistance' just for discoverability, but rely
   on the System` implementation. *)

RankDivergence::usage     = "RankDivergence[aligned, embA, embB] -> rows sorted by descending cosine distance.";

LLMExplain::usage         = "LLMExplain[row, opts] -> Association with structured tag + explanation.";
LLMProvider::usage        = "Option of LLMExplain. \"openai\" (default) | \"anthropic\".";

LLMTranslate::usage       = "LLMTranslate[greekVerse, ref, model:\"gpt-5.4\"] -> careful modern English translation.";
LLMNuanceCompare::usage   = "LLMNuanceCompare[<|greek,kjv,modern,ref,distance|>] -> Association with nuance analysis.";

OpenAIKey::usage          = "OpenAIKey[] -> the OpenAI API key from SystemCredential (or env).";

AvailableOpenAIModels::usage =
   "AvailableOpenAIModels[] -> sorted list of chat- and reasoning-capable OpenAI \
model IDs reachable from the configured key. Cached once per kernel session; \
AvailableOpenAIModels[Refresh -> True] forces a re-fetch.";

Begin["`Private`"];

(* Resolve the repo root at load time. $InputFileName is set during
   Get[]; using it inside a SetDelayed body would lose the binding
   by the time the function is called. *)
$pkgFile = $InputFileName;
$pkgDir  = If[StringQ[$pkgFile] && $pkgFile =!= "",
   DirectoryName[$pkgFile], Directory[]];
$repoRoot = ParentDirectory[$pkgDir];

repoRoot[] := $repoRoot;
dataDir[]  := FileNameJoin[{$repoRoot, "data"}];
embedDir[] := FileNameJoin[{dataDir[], "embeddings"}];

ensureDir[p_] := If[!DirectoryQ[p],
   CreateDirectory[p, CreateIntermediateDirectories -> True]];

(* =====================================================================
   Pair loading and alignment
   ===================================================================== *)

LoadPair[pairName_String, sideAFile_String, sideBFile_String] := Module[
   {a, b, aByRef, bByRef, commonRefs, aligned, onlyA, onlyB},
   a = Import[FileNameJoin[{dataDir[], sideAFile}], "RawJSON"];
   b = Import[FileNameJoin[{dataDir[], sideBFile}], "RawJSON"];
   aByRef = AssociationMap[#["ref"] -> # &, a] // Association;
   bByRef = AssociationMap[#["ref"] -> # &, b] // Association;
   (* AssociationMap returns rules; turn into an Association directly *)
   aByRef = AssociationThread[(#["ref"] & /@ a) -> a];
   bByRef = AssociationThread[(#["ref"] & /@ b) -> b];
   commonRefs = Intersection[Keys[aByRef], Keys[bByRef]];
   aligned = Map[
      <|"ref" -> #, "book" -> aByRef[#]["book"],
        "ch" -> aByRef[#]["ch"], "v" -> aByRef[#]["v"],
        "textA" -> aByRef[#]["text"], "textB" -> bByRef[#]["text"]|> &,
      commonRefs];
   onlyA = Complement[Keys[aByRef], Keys[bByRef]];
   onlyB = Complement[Keys[bByRef], Keys[aByRef]];
   <|"pair" -> pairName, "aligned" -> aligned,
     "onlyA" -> onlyA, "onlyB" -> onlyB,
     "nA" -> Length[a], "nB" -> Length[b]|>
];

ListPairs[] := {"greek-nt-kjv", "kjv-luther", "kjv-reina-valera",
   "iliad-pope-butler", "kapital-de-en", "tao-legge-goddard"};

(* =====================================================================
   Embedding API calls + on-disk cache
   ===================================================================== *)

(* Resolve the OpenAI API key from SystemCredential (preferred — set
   once in Mathematica, available to every script) with an env-var
   fallback for users running on a fresh machine. *)
OpenAIKey[] := Module[{k},
   k = Quiet @ SystemCredential["OPENAI_API_KEY"];
   If[!StringQ[k] || k === "",
     k = Environment["OPENAI_API_KEY"]];
   If[k === None || k === $Failed || k === "", Return[$Failed]];
   k];

resolveKey[provider_] := Switch[provider,
   "cohere",    With[{k = Quiet @ SystemCredential["COHERE_API_KEY"]},
                  If[StringQ[k] && k =!= "", k, Environment["COHERE_API_KEY"]]],
   "openai",    OpenAIKey[],
   "voyage",    With[{k = Quiet @ SystemCredential["VOYAGE_API_KEY"]},
                  If[StringQ[k] && k =!= "", k, Environment["VOYAGE_API_KEY"]]],
   "anthropic", With[{k = Quiet @ SystemCredential["ANTHROPIC_API_KEY"]},
                  If[StringQ[k] && k =!= "", k, Environment["ANTHROPIC_API_KEY"]]]];

Options[EmbedTexts] = {
   EmbeddingProvider -> "openai",  (* SystemCredential ships OpenAI on this machine *)
   EmbeddingModel    -> Automatic,
   "BatchSize"       -> 96,
   "InputType"       -> "search_document"  (* Cohere convention; ignored by OpenAI *)
};

defaultModel["cohere"]  = "embed-multilingual-v3.0";
defaultModel["openai"]  = "text-embedding-3-large";
defaultModel["voyage"]  = "voyage-3";

hashKey[s_String] := IntegerString[Hash[s, "SHA256"], 16, 64];

cachePath[provider_, model_] := (
   ensureDir[embedDir[]];
   FileNameJoin[{embedDir[], provider <> "_" <>
     StringReplace[model, {"." -> "_", "/" -> "_"}] <> ".mx"}]);

loadCache[path_] := If[FileExistsQ[path],
   Quiet @ Check[Get[path]; embCache, <||>],
   <||>];

saveCache[path_, assoc_] := (
   embCache = assoc;
   DumpSave[path, {embCache}]);

(* All three embedding helpers share the same 60-second URLRead
   TimeConstraint so a stalled HTTPS handshake (Mathematica's pool
   sometimes wedges after kernel-level interruptions) fails cleanly
   instead of hanging forever. *)

(* --- Cohere ------------------------------------------------------- *)
cohereEmbed[batch_List, model_, inputType_, key_] := Module[{resp, body},
   resp = URLRead[
      HTTPRequest[
        "https://api.cohere.com/v2/embed",
        <|"Method" -> "POST",
          "Headers" -> {
             "Authorization" -> "Bearer " <> key,
             "Content-Type"  -> "application/json",
             "Connection"    -> "close"},
          "Body" -> ExportString[<|
             "texts"      -> batch,
             "model"      -> model,
             "input_type" -> inputType,
             "embedding_types" -> {"float"}|>, "JSON"]|>],
      TimeConstraint -> 60];
   If[FailureQ[resp] || resp["StatusCode"] != 200,
     Print["Cohere error: ", If[FailureQ[resp], resp, resp["Body"]]];
     Return[$Failed]];
   body = ImportString[resp["Body"], "RawJSON"];
   body["embeddings"]["float"]
];

(* --- OpenAI ------------------------------------------------------- *)
openaiEmbed[batch_List, model_, key_] := Module[{resp, body},
   resp = URLRead[
      HTTPRequest[
        "https://api.openai.com/v1/embeddings",
        <|"Method" -> "POST",
          "Headers" -> {
             "Authorization" -> "Bearer " <> key,
             "Content-Type"  -> "application/json",
             "Connection"    -> "close"},
          "Body" -> ExportString[<|
             "input" -> batch, "model" -> model|>, "JSON"]|>],
      TimeConstraint -> 60];
   If[FailureQ[resp] || resp["StatusCode"] != 200,
     Print["OpenAI embedding error: ", If[FailureQ[resp], resp, resp["Body"]]];
     Return[$Failed]];
   body = ImportString[resp["Body"], "RawJSON"];
   (#["embedding"]) & /@ body["data"]
];

(* --- Voyage AI ---------------------------------------------------- *)
voyageEmbed[batch_List, model_, inputType_, key_] := Module[{resp, body},
   resp = URLRead[
      HTTPRequest[
        "https://api.voyageai.com/v1/embeddings",
        <|"Method" -> "POST",
          "Headers" -> {
             "Authorization" -> "Bearer " <> key,
             "Content-Type"  -> "application/json",
             "Connection"    -> "close"},
          "Body" -> ExportString[<|
             "input"      -> batch, "model" -> model,
             "input_type" -> inputType|>, "JSON"]|>],
      TimeConstraint -> 60];
   If[FailureQ[resp] || resp["StatusCode"] != 200,
     Print["Voyage error: ", If[FailureQ[resp], resp, resp["Body"]]];
     Return[$Failed]];
   body = ImportString[resp["Body"], "RawJSON"];
   (#["embedding"]) & /@ body["data"]
];

EmbedTexts[strings_List, opts:OptionsPattern[]] := Module[
   {provider, model, inputType, batchSize, key, path, cache,
    needed, vecs, batched, results, allVecs},
   provider  = OptionValue[EmbeddingProvider];
   model     = OptionValue[EmbeddingModel] /. Automatic :> defaultModel[provider];
   batchSize = OptionValue["BatchSize"];
   inputType = OptionValue["InputType"];

   key = resolveKey[provider];
   If[!StringQ[key] || key === "",
     Print["No API key for provider \"", provider,
           "\". Set SystemCredential[\"", ToUpperCase[provider],
           "_API_KEY\"] in Mathematica; see docs/api_keys.md."];
     Return[$Failed]];

   path = cachePath[provider, model];
   cache = loadCache[path];

   needed = Select[strings, !KeyExistsQ[cache, hashKey[#]] &];
   If[Length[needed] > 0,
     Print["  embedding ", Length[needed], " new strings (",
           Length[strings] - Length[needed], " cached) via ",
           provider, "/", model];
     batched = Partition[needed, UpTo[batchSize]];
     results = Map[
        Switch[provider,
          "cohere", cohereEmbed[#, model, inputType, key],
          "openai", openaiEmbed[#, model, key],
          "voyage", voyageEmbed[#, model, inputType, key]] &,
        batched];
     If[MemberQ[results, $Failed], Return[$Failed]];
     vecs = Flatten[results, 1];
     MapThread[(cache[hashKey[#1]] = #2) &, {needed, vecs}];
     saveCache[path, cache];
   ];
   allVecs = cache[hashKey[#]] & /@ strings;
   allVecs
];

(* =====================================================================
   Distance + ranking
   ===================================================================== *)

RankDivergence[aligned_List, embA_List, embB_List] := Module[{dists, rows},
   dists = MapThread[System`CosineDistance, {embA, embB}];
   rows = MapThread[Append[#1, "distance" -> #2] &, {aligned, dists}];
   ReverseSortBy[rows, #["distance"] &]
];

(* =====================================================================
   LLM hypothesis pass
   ===================================================================== *)

Options[LLMExplain] = {
   LLMProvider -> "openai",
   "Model"     -> Automatic,
   "MaxTokens" -> 4096,
   "Effort"    -> "high"
};

defaultLLM["anthropic"] = "claude-opus-4-7";
defaultLLM["openai"]    = "gpt-5.5";

rubric = "Reason categories (pick the single best match):
  theological-loading  — the English carries doctrinal weight the Greek doesn't (or vice versa)
  archaic-english       — Jacobean vocabulary or syntax has drifted (\"bowels\", \"wist\", \"prevent\")
  idiom-compression     — a Greek idiom is rendered word-for-word, or vice versa
  manuscript-variant    — the KJV translates a Byzantine reading absent from the SBLGNT
  word-order-prosody    — the meaning is preserved but the rhythm/word-order is heavily reshuffled
  proper-noun-aliasing  — names transliterated differently (\"Esaias\" vs \"Isaiah\", \"Jonas\" vs \"Jonah\")
  embedding-artifact    — the cosine gap is large but on inspection the verses say the same thing
  other                 — none of the above; explain in the paragraph";

buildPrompt[row_] := StringJoin[
   "You are auditing a single verse of a translation-pair alignment.\n",
   "Source: ", row["textA"], "\n",
   "Target: ", row["textB"], "\n",
   "Reference: ", row["ref"], "\n",
   "Cosine distance between multilingual-embedding vectors: ",
   ToString @ NumberForm[row["distance"], {4, 3}], "\n\n",
   rubric, "\n\n",
   "Reply with exactly this JSON shape (no surrounding prose):\n",
   "{\n",
   "  \"category\": \"<one of the categories above>\",\n",
   "  \"explanation\": \"<one paragraph, 60-120 words, concrete and falsifiable>\"\n",
   "}"];

anthropicCall[prompt_String, model_, maxTok_, key_] := Module[{resp, body},
   resp = URLRead[
      HTTPRequest[
        "https://api.anthropic.com/v1/messages",
        <|"Method" -> "POST",
          "Headers" -> {
             "x-api-key" -> key,
             "anthropic-version" -> "2023-06-01",
             "Content-Type" -> "application/json",
             "Connection"   -> "close"},
          "Body" -> ExportString[<|
             "model"      -> model,
             "max_tokens" -> maxTok,
             "messages"   -> {<|"role" -> "user", "content" -> prompt|>}|>,
             "JSON"]|>],
      TimeConstraint -> 240];
   If[FailureQ[resp] || resp["StatusCode"] != 200,
     Print["Anthropic error: ", If[FailureQ[resp], resp, resp["Body"]]];
     Return[$Failed]];
   body = ImportString[resp["Body"], "RawJSON"];
   First[body["content"]]["text"]
];

(* openaiCallCurl — bypass Mathematica's libcurl pool entirely by
   shelling out to the system curl binary via RunProcess[].

   WHY RunProcess[] INSTEAD OF Run[]:
   Run[] on macOS does not reliably reap child processes after the first
   invocation in a kernel session; the second (and later) calls block
   indefinitely even when curl's --max-time fires. Root causes include:
     1. Run[] on macOS uses /bin/sh -c, and the kernel may leave the
        shell wrapper process alive, preventing SIGCHLD delivery.
     2. The process-table slot is never freed, so the third call blocks
        waiting for a slot that is never released.
     3. --max-time fires inside the curl child but the supervising sh
        wrapper may still be waiting, blocking WL's wait4() call.
   RunProcess[] uses a direct execve()-family call (no shell wrapper),
   captures stdout/stderr into strings, supports a real TimeConstraint
   option that sends SIGKILL to the whole process group when it fires,
   and is fully synchronous with a well-defined exit code.  Temp-file
   round-trips are eliminated: the JSON payload is passed via stdin
   (RunProcess third argument), which also avoids any shell-quoting
   hazard with Unicode. *)
openaiCallCurl[payloadStr_String, key_String, timeout_Integer:60,
               url_String:"https://api.openai.com/v1/chat/completions",
               method_String:"POST"] :=
Module[{curlBin, args, result, exitCode, bodyStr, t0, isGet},
   (* Locate curl; prefer /usr/bin/curl which ships with macOS. *)
   curlBin = SelectFirst[
      {"/usr/bin/curl", "/opt/homebrew/bin/curl", "/usr/local/bin/curl"},
      FileExistsQ, "curl"];
   t0 = AbsoluteTime[];
   isGet = ToUpperCase[method] === "GET";
   args = Join[
      {"-s", "-X", ToUpperCase[method],
       "--max-time", ToString[timeout],
       "-H", "Authorization: Bearer " <> key,
       "-H", "Content-Type: application/json"},
      (* POST sends the payload from stdin; GET has no body. *)
      If[isGet, {}, {"--data-binary", "@-"}],
      {url}];
   (* RunProcess third arg is the stdin string.
      TimeConstrained[] wraps the call so that if RunProcess itself
      blocks past curl's --max-time (e.g. the process-table slot is
      never released), the kernel aborts and returns $TimedOut.
      We give 5 extra seconds beyond curl's own --max-time budget. *)
   result = TimeConstrained[
      RunProcess[
         {curlBin, Sequence @@ args},
         All,
         If[isGet, "", payloadStr]    (* stdin = JSON payload (POST only) *)
      ],
      timeout + 5,
      $TimedOut];
   (* ProcessExited? guard: if TimeConstrained fired it returns
      $TimedOut rather than an Association. *)
   If[result === $TimedOut,
     Print["openaiCallCurl: RunProcess timed out after ",
           Round[AbsoluteTime[] - t0], " s (model may be overloaded)."];
     Return[$Failed]];
   If[!AssociationQ[result],
     Print["openaiCallCurl: unexpected RunProcess return: ", result];
     Return[$Failed]];
   exitCode = result["ExitCode"];
   If[exitCode =!= 0,
     Print["openaiCallCurl: curl exited ", exitCode,
           " after ", Round[AbsoluteTime[] - t0, 0.1], " s. stderr: ",
           StringTake[result["StandardError"], UpTo[300]]];
     Return[$Failed]];
   bodyStr = result["StandardOutput"];
   If[!StringQ[bodyStr] || StringLength[bodyStr] == 0,
     Print["openaiCallCurl: empty response body from curl (stderr: ",
           StringTake[result["StandardError"], UpTo[300]], ")"];
     Return[$Failed]];
   Print["openaiCallCurl: OK in ", Round[AbsoluteTime[] - t0, 0.1], " s, ",
         StringLength[bodyStr], " bytes"];
   bodyStr
];

(* isReasoningModel — reasoning_effort is only accepted by o-series and
   gpt-5.5 / gpt-5.5-pro. For all other models (including gpt-5.4 and
   gpt-4.1) the parameter is rejected with a 400 error. *)
isReasoningModel[m_String] :=
   StringMatchQ[m, ___ ~~ ("o1" | "o3" | "o4" | "5.5") ~~ ___,
                IgnoreCase -> True];

(* ------------------------------------------------------------------
   AvailableOpenAIModels[] — GET /v1/models, filter to chat/reasoning
   model IDs, cache for the rest of the kernel session.

   Use AvailableOpenAIModels[Refresh -> True] to force a re-fetch.
   ------------------------------------------------------------------ *)
$openaiModelCache = None;

Options[AvailableOpenAIModels] = {Refresh -> False};

AvailableOpenAIModels[OptionsPattern[]] := Module[
   {key, raw, body, ids, keep},
   If[$openaiModelCache =!= None && !TrueQ[OptionValue[Refresh]],
      Return[$openaiModelCache]];
   key = OpenAIKey[];
   If[!StringQ[key] || key === "",
      Print["AvailableOpenAIModels: no OpenAI key in SystemCredential."];
      Return[{"gpt-5.4", "gpt-5.5"}]];
   raw = openaiCallCurl[
      "", key, 30,
      "https://api.openai.com/v1/models",
      "GET"];
   If[raw === $Failed,
      Print["AvailableOpenAIModels: fetch failed; returning a static fallback list."];
      Return[$openaiModelCache = {"gpt-5.4", "gpt-5.5"}]];
   body = Quiet @ ImportString[raw, "RawJSON"];
   If[!AssociationQ[body] || !KeyExistsQ[body, "data"],
      Print["AvailableOpenAIModels: unexpected response shape; returning fallback."];
      Return[$openaiModelCache = {"gpt-5.4", "gpt-5.5"}]];
   ids = Sort[(#["id"]) & /@ body["data"]];
   (* Keep only chat/reasoning families that we know how to call. *)
   keep = Select[ids,
      StringMatchQ[#,
        ("gpt-" | "o1" | "o3" | "o4" | "chatgpt-") ~~ ___] &];
   (* Drop image/audio/embedding/realtime/search variants that wouldn't
      work in this pipeline. *)
   keep = Select[keep,
      !StringContainsQ[#,
        "image" | "audio" | "tts" | "whisper" | "embed" | "realtime" | "search"] &];
   $openaiModelCache = keep
];

openaiCall[prompt_String, model_, maxTok_, key_, effort_:"high",
           system_:None, jsonMode_:False] := Module[
   {messages, payload, payloadStr, bodyStr, body},
   messages = If[system === None,
      {<|"role" -> "user", "content" -> prompt|>},
      {<|"role" -> "system", "content" -> system|>,
       <|"role" -> "user",   "content" -> prompt|>}];
   payload = <|
      "model"                 -> model,
      "messages"              -> messages,
      "max_completion_tokens" -> maxTok|>;
   (* Only reasoning-capable models accept reasoning_effort; adding it
      to a standard chat model (e.g. gpt-5.4, gpt-4.1) causes a 400. *)
   If[isReasoningModel[model],
      payload = Append[payload, "reasoning_effort" -> effort]];
   If[jsonMode,
      payload = Append[payload, "response_format" -> <|"type" -> "json_object"|>]];
   payloadStr = ExportString[payload, "JSON"];

   (* Primary transport: system curl subprocess, bypasses WL HTTP pool.
      This is the only transport that reliably completes from inside a
      DynamicModule Button where URLRead can hang on a stale socket. *)
   bodyStr = openaiCallCurl[payloadStr, key, 60];

   (* Fallback: URLRead (works fine from wolframscript / fresh kernels) *)
   If[bodyStr === $Failed,
     Print["openaiCallCurl failed; falling back to URLRead."];
     With[{resp = TimeConstrained[
            URLRead[
              HTTPRequest[
                "https://api.openai.com/v1/chat/completions",
                <|"Method" -> "POST",
                  "Headers" -> {
                     "Authorization" -> "Bearer " <> key,
                     "Content-Type"  -> "application/json",
                     "Connection"    -> "close"},
                  "Body" -> payloadStr|>],
              TimeConstraint -> 60],
            60, $Failed]},
       If[resp === $Failed,
         Print["OpenAI chat call timed out after 60 s."];
         Return[$Failed]];
       If[FailureQ[resp] || resp["StatusCode"] != 200,
         Print["OpenAI error: ", If[FailureQ[resp], resp, resp["Body"]]];
         Return[$Failed]];
       bodyStr = resp["Body"]]];

   body = Quiet @ ImportString[bodyStr, "RawJSON"];
   If[!AssociationQ[body] || !KeyExistsQ[body, "choices"],
     (* Some GPT-5.x responses contain raw surrogate halves that WL's
        JSON importer rejects; extract content field with a regex. *)
     With[{m = StringCases[bodyStr,
              RegularExpression["\"content\"\\s*:\\s*\"((?:\\\\.|[^\"\\\\])*)\""] :> "$1", 1]},
       If[m === {}, Print["OpenAI: could not parse response body: ",
                          StringTake[bodyStr, UpTo[200]]];
                    Return[$Failed]];
       Return[StringReplace[First[m],
          {"\\n" -> "\n", "\\\"" -> "\"", "\\t" -> "\t", "\\\\" -> "\\"}]]]];
   First[body["choices"]]["message"]["content"]
];

(* Pull a single string-valued field out of a JSON-shaped string with a
   regex. Robust to embedded literal newlines (which a strict JSON parser
   would reject) and to weird Unicode. *)
extractField[s_String, field_String] := Module[{m},
   m = StringCases[s,
      RegularExpression["\"" <> field <> "\"\\s*:\\s*\"((?:\\\\.|[^\"\\\\])*)\""]
         :> "$1", 1];
   If[m === {}, "",
      StringReplace[First[m],
         {"\\n" -> "\n", "\\\"" -> "\"", "\\t" -> "\t", "\\\\" -> "\\"}]]];

(* Pull a string-array field "key": ["a","b",...] out of a JSON-shaped
   string. Returns a list of strings (possibly empty). *)
extractArrayField[s_String, field_String] := Module[{m, items},
   m = StringCases[s,
      RegularExpression["\"" <> field <> "\"\\s*:\\s*\\[((?:\\\\.|[^\\]\\\\])*)\\]"]
         :> "$1", 1];
   If[m === {}, Return[{}]];
   items = StringCases[First[m],
      RegularExpression["\"((?:\\\\.|[^\"\\\\])*)\""] :> "$1"];
   StringReplace[#, {"\\n" -> "\n", "\\\"" -> "\"", "\\\\" -> "\\"}] & /@ items];

(* Extract every "key": "string-value" pair from a JSON-shaped string.
   Lets parseJSONLoose's fallback work for any schema, not just the
   nuance schema. Array-valued fields (cross_refs) are extracted via a
   separate pass. *)
extractAllStringFields[s_String] := Module[{m},
   m = StringCases[s,
     RegularExpression["\"([A-Za-z_][A-Za-z_0-9]*)\"\\s*:\\s*\"((?:\\\\.|[^\"\\\\])*)\""]
        :> {"$1", "$2"}];
   Association[
      #[[1]] -> StringReplace[#[[2]],
         {"\\n" -> "\n", "\\\"" -> "\"", "\\t" -> "\t", "\\\\" -> "\\"}] & /@ m]];

parseJSONLoose[s_String] := Module[{json, i, j, parsed, fallback},
   i = StringPosition[s, "{"];
   j = StringPosition[s, "}"];
   If[i =!= {} && j =!= {},
     json = StringTake[s, {First[First[i]], Last[Last[j]]}];
     parsed = Quiet @ ImportString[json, "RawJSON"];
     If[AssociationQ[parsed] && Length[parsed] > 0, Return[parsed]]];
   (* Strict parse failed (likely a non-BMP codepoint). Fall back to a
      schema-agnostic regex extraction of every string-valued field plus
      array-valued cross_refs. *)
   fallback = extractAllStringFields[s];
   If[StringContainsQ[s, "cross_refs"],
     fallback = Append[fallback, "cross_refs" -> extractArrayField[s, "cross_refs"]]];
   fallback
];

LLMExplain[row_, opts:OptionsPattern[]] := Module[
   {provider, model, maxTok, effort, key, prompt, raw},
   provider = OptionValue[LLMProvider];
   model    = OptionValue["Model"] /. Automatic :> defaultLLM[provider];
   maxTok   = OptionValue["MaxTokens"];
   effort   = OptionValue["Effort"];
   key = resolveKey[provider];
   If[!StringQ[key] || key === "",
     Print["No API key for LLM provider \"", provider, "\"."];
     Return[$Failed]];
   prompt = buildPrompt[row];
   raw = Switch[provider,
      "anthropic", anthropicCall[prompt, model, maxTok, key],
      "openai",    openaiCall[prompt, model, maxTok, key, effort, None, True]];
   If[raw === $Failed, Return[$Failed]];
   Append[row, "hypothesis" -> parseJSONLoose[raw]]
];


(* =====================================================================
   LLMTranslate — feed a Koine Greek verse to GPT-5.4 (no reasoning
   overhead) and ask for a careful modern English translation. Returns
   the translation string, or $Failed.

   Model choice: gpt-5.4 is a standard chat model — it responds in
   ~2-3 s vs. 15-60 s for gpt-5.5 with reasoning_effort="medium".
   Translation is a single-pass task where reasoning tokens add latency
   without improving quality; gpt-5.4 is fully capable of nuanced NT
   Greek translation without a thinking phase.
   ===================================================================== *)

translateSystem = "You are a New Testament Greek scholar producing a \
fresh modern English translation calibrated for an academic audience \
(religious studies, classics, biblical philology). Aim for an NRSVue-style \
register: literal where the Greek is concrete, idiomatic where literal \
would mislead, but never paraphrase. Specifically: \n\
  - preserve Greek tense / voice / mood distinctions when they matter \
(aorist vs perfect, middle vs passive); \n\
  - preserve Greek word order when it carries emphasis (verb-fronting, \
fronted predicates); \n\
  - translate \[Beta]\[Alpha]\[Pi]\[Tau]\[Iota]\[Zeta]\[Omega] as \"baptize\" only when it is a \
technical religious term; otherwise \"immerse\"; \n\
  - translate \[Sigma]\[Alpha]\[Rho]\[Xi] as \"flesh\" when Pauline-technical, otherwise \
\"body\" / \"mortal nature\"; \n\
  - translate \[Pi]\[Nu]\[CurlyEpsilon]\[Upsilon]\[Mu]\[Alpha] as \"Spirit\" (capital) when divine, \
\"spirit\" otherwise; \n\
  - do not import doctrinal weight the Greek does not bear; \n\
  - do not add words the Greek does not contain (no parentheticals). \n\n\
Reply with the English translation only \[Dash] no commentary, no quotation \
marks, no verse number, no leading whitespace.";

LLMTranslate[greek_String, ref_:"", model_String:"gpt-5.4"] := Module[
   {key, prompt, raw},
   key = OpenAIKey[];
   If[!StringQ[key] || key === "",
     Print["LLMTranslate: no OpenAI key."]; Return[$Failed]];
   prompt = If[ref === "",
      greek,
      ref <> ": " <> greek];
   (* Default model is gpt-5.4 (no reasoning phase, ~2-3 s response).
      gpt-5.5 gives slightly more thorough output at the cost of 15-60 s
      of "thinking" before any tokens appear. The effort arg is passed
      but ignored by openaiCall for non-reasoning models
      (isReasoningModel["gpt-5.4"] -> False). *)
   raw = openaiCall[prompt, model, 4000, key, "medium", translateSystem, False];
   If[raw === $Failed, Return[$Failed]];
   StringTrim[raw]
];


(* =====================================================================
   LLMNuanceCompare — given Greek + KJV + a modern translation +
   cosine distance, ask GPT-5.5 to articulate the *nuances* of
   difference between the KJV and the modern rendering. Focus on
   what would be invisible at a glance.

   Returns an Association with structured fields.
   ===================================================================== *)

nuanceSystem = "You are a New Testament textual and philological critic \
writing for an audience of academics in religious studies, classical \
philology, and intellectual history. You read Koine Greek fluently and \
work from the standard apparatus: Nestle\[Dash]Aland 28 with the SBLGNT \
text, the BDAG lexicon, BDF grammar, Wallace's Greek Grammar Beyond the \
Basics, and Metzger's Textual Commentary on the Greek New Testament. You \
also know the Oxford English Dictionary on 17th-century English usage. \n\n\
The user gives you ONE verse rendered three ways: the Koine Greek source \
(SBLGNT), the 1611/1769 KJV, and a careful modern English translation. \
Your job is to articulate the NUANCES \[Dash] the small but real \
differences between the KJV and the modern rendering that a casual reader \
misses, and that are useful to a scholar deciding which rendering to \
quote and why. \n\n\
Skip purely cosmetic differences (\"thou\"/\"you\", capitalisation of \
pronouns referring to Deity, \"begat\"/\"fathered\", \"unto\"/\"to\"). \
Focus on things that move the meaning. Address (where applicable, in \
this order of priority): \n\
  1. Textual criticism. Is the KJV translating a Textus Receptus / \
Byzantine reading absent from the SBLGNT? Name the specific Greek words \
the KJV is adding or substituting, the families of witnesses on each side \
where you can (Sinaiticus, Vaticanus, Alexandrinus vs the Majority Text), \
and whether the variant is in Metzger's commentary. \n\
  2. Lexical drift in 17th-c. English. The KJV's word may have meant \
something different in 1611 than today. Classic cases: \"prevent\" = go \
before, \"conversation\" = manner of life (\[Pi]\[Omicron]\[Lambda]\[Iota]\[Tau]\[Epsilon]\[CurlyEpsilon]\[Mu]\[Alpha]), \
\"bowels\" = inward affections (\[Sigma]\[Pi]\[Lambda]\[Alpha]\[Gamma]\[Chi]\[Nu]\[Alpha]), \
\"charity\" = self-giving love (\[Alpha]\[Gamma]\[Alpha]\[Pi]\[Eta]), \"meat\" = food. Name the OED sense. \n\
  3. Theological loading. Does the KJV's choice carry a doctrinal weight \
the Greek does not strictly require (or vice versa)? Examples: \
\"justified\" (\[Delta]\[Iota]\[Kappa]\[Alpha]\[Iota]\[Omicron]\[Omega]) vs \
\"reckoned righteous\"; \"baptize\" (a transliteration that hides \[Beta]\[Alpha]\[Pi]\[Tau]\[Iota]\[Zeta]\[Omega] = \
\"immerse\"); \"hell\" rendering three different Greek words. \n\
  4. Syntactic compression / word order. Greek tense / voice / mood is \
often flattened in English. If the Greek has an aorist where the KJV uses \
a perfect, or a middle voice where the KJV uses a passive, say so. Note \
emphatic word order (verb-fronting, fronted predicates) when it matters. \n\
  5. Idiom handling and Hebraisms. The KJV sometimes calques a Greek (or \
underlying Aramaic) idiom literally while the modern reads idiomatically, \
or vice versa: \"hardness of heart\" (\[Sigma]\[Kappa]\[Lambda]\[Eta]\[Rho]\[Omicron]\[Kappa]\[Alpha]\[Rho]\[Delta]\[Iota]\[Alpha]), \
\"answered and said\" (\[Alpha]\[Pi]\[Omicron]\[Kappa]\[Rho]\[Iota]\[Theta]\[CurlyEpsilon]\[Iota]\[Sigma] \[Epsilon]\[Iota]\[Pi]\[Epsilon]\[Nu]). \n\
  6. Proper-noun transliteration. \"Esaias\"/\"Isaiah\", \"Elias\"/ \
\"Elijah\", \"Jonas\"/\"Jonah\" \[LongDash] flag and move on. \n\n\
Be concrete and falsifiable. Quote Greek lemmas in Greek script. Where \
relevant, gesture at a standard reference (\"BDAG s.v. \[Pi]\[Lambda]\[Eta]\[Rho]\[Omega]\[Mu]\[Alpha]\", \
\"Metzger 2nd ed. p. 456\", \"BDF \[Section]400\") \[LongDash] but only if you are confident \
the citation is real; do not invent page numbers. Aim for 120\[Dash]180 words \
of substance, not 300, and not generic platitudes.";

LLMNuanceCompare[row_Association] := Module[{key, prompt, raw, parsed},
   key = OpenAIKey[];
   If[!StringQ[key] || key === "",
     Print["LLMNuanceCompare: no OpenAI key."]; Return[$Failed]];
   prompt = StringJoin[
     "Verse: ", row["ref"], "\n",
     "Greek (SBLGNT): ", row["greek"], "\n",
     "KJV (1769): ", row["kjv"], "\n",
     "Modern (your prior careful translation): ", row["modern"], "\n",
     "Cosine distance KJV vs modern (text-embedding-3-large): ",
     ToString @ NumberForm[row["distance"], {4, 3}], "\n\n",
     "Return a single JSON object with exactly these fields (no surrounding text, no markdown fences):\n",
     "{\n",
     "  \"primary_nuance\": \"<one sentence; the single most interesting difference a scholar would flag>\",\n",
     "  \"category\": \"<textual-variant | archaic-english-drift | theological-loading | syntactic-compression | idiom-or-hebraism | proper-noun-transliteration | translation-style | other>\",\n",
     "  \"key_term\": \"<the Greek lemma in Greek script that drives the nuance, or empty string>\",\n",
     "  \"english_drift\": \"<if applicable: the KJV English word + its 1611 sense + its modern sense; else empty string>\",\n",
     "  \"textual_note\": \"<if applicable: which witnesses go each way, citing Metzger when you are sure of the citation; else empty string>\",\n",
     "  \"cross_refs\": [\"<other NT verses that illuminate this one; max 3>\"],\n",
     "  \"explanation\": \"<120\[Dash]180 words; scholarly tone; concrete, falsifiable, quotes the Greek where it matters, names the standard reference work in passing where natural>\"\n",
     "}"];
   (* High reasoning effort burns many tokens before the visible reply
      starts; 8000 is empirical headroom for one verse. *)
   raw = openaiCall[prompt, "gpt-5.5", 8000, key, "high", nuanceSystem, False];
   If[raw === $Failed, Return[$Failed]];
   If[StringQ[raw] && StringTrim[raw] === "",
     Print["    [empty response \[LongDash] model exhausted reasoning budget; retry with medium effort]"];
     raw = openaiCall[prompt, "gpt-5.5", 4000, key, "medium", nuanceSystem, False];
     If[raw === $Failed || StringTrim[raw] === "", Return[$Failed]]];
   parsed = parseJSONLoose[raw];
   Join[row, <|"nuance" -> parsed, "raw" -> raw|>]
];

End[];
EndPackage[];
