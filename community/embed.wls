#!/usr/bin/env wolframscript
(* ::Package:: *)

(*  Embed both sides of a configured translation pair via the
    multilingual API of choice and write the resulting vectors as
    a single .mx file under data/embeddings/.

    Usage:
        wolframscript -file wolfram/embed.wls -- <pair-name>
            pair-name defaults to "greek-nt-kjv"

    Pair registry: see pairFiles[] below.

    Reads API keys from environment (COHERE_API_KEY by default).
    Cohere v3.0 multilingual gives consistent cross-language scores
    for the headline pair; see docs/api_keys.md.
*)

repoRoot = ParentDirectory @ DirectoryName[$InputFileName];
Get @ FileNameJoin[{repoRoot, "wolfram", "framework.wl"}];

pairFiles[name_] := Switch[name,
   "greek-nt-kjv",     {"greek_nt.json",  "kjv_nt.json"},
   "kjv-luther",       {"kjv_nt.json",    "luther_nt.json"},
   "kjv-reina-valera", {"kjv_nt.json",    "reina_valera_nt.json"},
   "iliad-pope-butler",{"iliad_pope.json","iliad_butler.json"},
   "kapital-de-en",    {"kapital_de.json","kapital_en.json"},
   "tao-legge-goddard",{"tao_legge.json", "tao_goddard.json"},
   _, $Failed];

args = DeleteCases[Rest[$ScriptCommandLine], "--"];
pair = If[Length[args] >= 1, args[[1]], "greek-nt-kjv"];

files = pairFiles[pair];
If[files === $Failed,
   Print["Unknown pair: ", pair, ". Known: ", TPFramework`ListPairs[]];
   Exit[2]];

Print["=== Embedding pair: ", pair, " ==="];
Print["  side A: ", files[[1]], "    side B: ", files[[2]]];

If[!FileExistsQ[FileNameJoin[{repoRoot, "data", files[[1]]}]] ||
   !FileExistsQ[FileNameJoin[{repoRoot, "data", files[[2]]}]],
   Print["Missing input JSON. Run the fetcher first."];
   Exit[3]];

loaded = TPFramework`LoadPair[pair, files[[1]], files[[2]]];
aligned = loaded["aligned"];
Print["  aligned units: ", Length[aligned]];

textsA = #["textA"] & /@ aligned;
textsB = #["textB"] & /@ aligned;

provider = Environment["TP_EMBED_PROVIDER"];
If[provider === None || provider === "", provider = "cohere"];
Print["  provider: ", provider];

embA = TPFramework`EmbedTexts[textsA,
   TPFramework`EmbeddingProvider -> provider,
   "InputType" -> "search_document"];
If[embA === $Failed, Print["embedding side A failed"]; Exit[4]];

embB = TPFramework`EmbedTexts[textsB,
   TPFramework`EmbeddingProvider -> provider,
   "InputType" -> "search_document"];
If[embB === $Failed, Print["embedding side B failed"]; Exit[4]];

(* Write a paired .mx so the analyze step does not have to re-load the
   per-provider cache and re-resolve hashes. *)
out = FileNameJoin[{repoRoot, "data", "embeddings",
   pair <> "_" <> provider <> ".mx"}];
embedPair = <|
   "pair" -> pair, "provider" -> provider,
   "aligned" -> aligned,
   "embA" -> embA, "embB" -> embB,
   "onlyA" -> loaded["onlyA"], "onlyB" -> loaded["onlyB"],
   "createdAt" -> DateString[]|>;
DumpSave[out, embedPair];
Print["  wrote ", out, " (", FileByteCount[out], " bytes)"];
