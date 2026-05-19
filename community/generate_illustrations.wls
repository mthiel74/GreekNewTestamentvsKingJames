#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Generate the conceptual / infographic illustrations for the
    Wolfram Community notebook via gpt-image-2 (called over HTTP
    from pure Wolfram \[Dash] no Python anywhere).

    Outputs to docs/images/figures-generated/*.png. Committed so a
    rebuild of the notebook does not need to call OpenAI again.

    Cost: 3 medium-quality 1024x1024 images = ~$0.12.

    Re-run:
        wolframscript -file wolfram/generate_illustrations.wls
*)

$CharacterEncoding = "UTF-8";

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
outDir   = FileNameJoin[{repoRoot, "docs", "images", "figures-generated"}];
If[!DirectoryQ[outDir], CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

(* Inline the gpt-image-2 caller so this script is self-contained. *)
callGPTImage2[myPrompt_String, myAPIKey_String,
              size_:"1024x1024", quality_:"medium"] :=
 Module[{jsonBody, myRequest, response, body, b64, bytes},
  jsonBody = ExportString[<|
     "model" -> "gpt-image-2", "prompt" -> myPrompt, "n" -> 1,
     "size" -> size, "quality" -> quality|>, "JSON"];
  myRequest = HTTPRequest[
     "https://api.openai.com/v1/images/generations",
     <|"Method" -> "POST", "Body" -> jsonBody,
       "ContentType" -> "application/json",
       "Headers" -> {"Authorization" -> "Bearer " <> myAPIKey}|>,
     TimeConstraint -> 240];
  response = URLRead[myRequest];
  If[FailureQ[response] || response["StatusCode"] != 200,
     Print["  callGPTImage2 error (", response["StatusCode"], "): ",
           StringTake[response["Body"], UpTo[400]]];
     Return[$Failed]];
  body = ImportString[response["Body"], "RawJSON"];
  b64 = First[body["data"]]["b64_json"];
  If[!StringQ[b64], Print["  no b64_json"]; Return[$Failed]];
  ImportByteArray[ByteArray[ImportString[b64, {"Base64", "Byte"}]], "PNG"]];

key = SystemCredential["OPENAI_API_KEY"];
If[!StringQ[key] || key === "",
   Print["No OpenAI key in SystemCredential"]; Exit[2]];

(* --- The illustrations ----------------------------------- *)

prompts = <|
  "hero_cover" -> "A wide cover illustration for a long-form scientific \
notebook titled \"Lost in Translation\". Wide 16:9 aspect-ratio composition. \
On the left half: an old open codex with two visible pages of dense calligraphic \
text \[Dash] one page in faint Koine Greek, one page in 17th-century English. \
On the right half: a thin modern smartphone showing a contemporary newsfeed \
with brief English paragraphs. Between them, faint horizontal lines of varying \
length connect matched passages \[Dash] some short (close match), some long \
(distant rendering), drawn in a muted terracotta. The background is warm cream \
parchment fading into pale slate on the right. Above the whole scene, a thin \
title bar in elegant serif: \"Lost in Translation\". Below the codex/phone \
pair, in small caps: \"A Wolfram framework for auditing translation nuance\". \
Flat, vector-style, no photographic elements, no AI-rendered faces, no human \
figures. Muted scholarly palette: cream, navy, terracotta, sage. 16:9 wide \
landscape composition, ~1536x832 if available, else 1024x1024 centered.",

  (* OPTIONAL ROUND 1 illustrations, kept for re-generation *)
  "pipeline_diagram" -> "A clean, flat-design horizontal infographic of a four-stage \
analysis pipeline for a scientific blog post. Four labeled rounded-rectangle \
boxes arranged left-to-right and connected by thin arrows. Box 1 labeled \
'Greek source verse' shows a small scroll icon. Box 2 labeled 'GPT-5.5 modern \
translation' shows a neural-network node icon. Box 3 labeled 'Embed both \
(KJV + modern) text-embedding-3-large' shows a small dot-cloud icon. Box 4 \
labeled 'Cosine distance + GPT-5.5 nuance analysis' shows a small ranked-list \
icon. Above the whole row, a title bar reads 'Translation-pair divergence \
framework'. Muted scholarly palette: cream background, navy boxes with white \
text, terracotta accents on the arrows. No photographic elements. No hand-drawn \
elements. Crisp vector-style edges. 1024x1024 square, with the pipeline \
horizontally centered.",

  "embedding_space"  -> "A scientific conceptual diagram for a religious-studies \
blog post: a 2D scatter plot illustrating that an off-the-shelf multilingual \
embedder clusters texts BY LANGUAGE more than BY MEANING. On the left, a cluster \
of dots labeled 'Greek verses (SBLGNT)' in navy. On the right, two overlapping \
clusters labeled 'KJV English' (red) and 'modern English' (orange). A double-headed \
arrow between the Greek cluster and the English clusters labeled 'cosine \
distance dominated by language, not meaning'. Below: a second small inset \
showing the KJV-vs-modern distance within the English cluster, with the legend \
'within-language distance = the actual nuance signal'. Flat, scholarly, \
infographic style. Cream background. Crisp labels. No clip-art, no faces, no \
photographic elements. 1024x1024 square.",

  "nuance_taxonomy"  -> "A scholarly infographic for a New Testament translation \
analysis project, in the style of a typology diagram. A central title at the top \
reads 'Categories of KJV vs modern English divergence'. Below it, seven labelled \
panels arranged in a tidy 3-3-1 grid, each panel a flat-design pictogram with a \
short label. Labels (in this order): 'textual variant', 'archaic English drift', \
'theological loading', 'syntactic compression', 'idiom / Hebraism', \
'proper-noun transliteration', 'translation style'. Each panel has a small \
flat icon: a manuscript page (textual variant), a feather quill on old paper \
(archaic English), a cross (theological loading), an arrow being compressed \
(syntactic compression), an open book with two languages (idiom), a name plate \
(proper noun), a writer at a desk (style). Muted scholarly palette: cream \
background, navy text, terracotta and sage accents. Crisp flat-vector \
illustration style. No photographic realism, no AI-style faces. 1024x1024 square."
|>;

(* Optional CLI: pass one or more key names to only regenerate those.
   `wolframscript -file generate_illustrations.wls -- hero_cover`
   With no args, regenerate all of them. *)
args = DeleteCases[Rest[$ScriptCommandLine], "--"];
selectedPrompts = If[args === {}, prompts,
   KeyTake[prompts, args]];

Print["Generating ", Length[selectedPrompts], " illustration(s) via gpt-image-2 ..."];

(* Hero is wide; others are square. *)
sizeFor[name_] :=
   If[name === "hero_cover", "1536x1024", "1024x1024"];

results = KeyValueMap[
  Function[{name, prompt},
    Print["\n[", name, "] ..."];
    With[{img = callGPTImage2[prompt, key, sizeFor[name], "medium"]},
      If[Head[img] === Image,
        Export[FileNameJoin[{outDir, name <> ".png"}], img];
        Print["  wrote ", name, ".png"];
        name -> "ok",
        name -> "failed"]]],
  selectedPrompts];

Print["\n=== summary ==="];
Print[results];
