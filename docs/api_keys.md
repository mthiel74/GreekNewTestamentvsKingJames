# API keys

The pipeline reads keys from **Wolfram `SystemCredential[]`** at
run time (set once in Mathematica, persisted across sessions and
shared with every `wolframscript`). An environment-variable fallback
exists for portability. Keys are never read from a committed file.

To set a key in Wolfram (one-time, in a notebook or `wolframscript`):

```mathematica
SetSystemCredential["OPENAI_API_KEY", "sk-..."]
```

After that any script in this repo can call
`SystemCredential["OPENAI_API_KEY"]` and get the same value.

## Embeddings

Pick **one** of:

| env var           | provider | model                          | per-1M-token cost (approx) |
| ----------------- | -------- | ------------------------------ | -------------------------- |
| `COHERE_API_KEY`  | Cohere   | `embed-multilingual-v3.0`      | $0.10                      |
| `OPENAI_API_KEY`  | OpenAI   | `text-embedding-3-large`       | $0.13                      |
| `VOYAGE_API_KEY`  | Voyage   | `voyage-3` (multilingual)      | $0.06                      |

The framework's default is Cohere — it was trained explicitly for
cross-lingual retrieval and handles Koine Greek + KJV English in a
single embedding space.

## LLM hypothesis pass

Pick **one** of:

| env var               | provider  | model                          |
| --------------------- | --------- | ------------------------------ |
| `ANTHROPIC_API_KEY`   | Anthropic | `claude-opus-4-7` (default)   |
| `OPENAI_API_KEY`      | OpenAI    | `gpt-5` or `gpt-5.4`           |

The LLM is asked to classify each top-N divergent verse into one of
a fixed rubric of reasons (theological loading, archaic English,
manuscript variant, idiomatic compression, …) and to write a
one-paragraph explanation. Cost scales with how many divergent
verses you analyse — default 50, total ≈ $0.10 with Claude Opus.

## Loading the keys

In a fresh shell:

```sh
export COHERE_API_KEY=co-...
export ANTHROPIC_API_KEY=sk-ant-...
wolframscript -file wolfram/run_all.wls
```

Or, drop a `.env`-style file at the repo root (git-ignored) and
source it. The Wolfram scripts only call `Environment[]`; they do
not parse `.env` themselves.

## What happens if no keys are set

* The fetchers run fine — they only hit GitHub and Project Gutenberg.
* `embed.wls` exits with a clear error pointing at this file.
* The notebook build still works *if* a cached embedding `.mx` file
  is present in `data/embeddings/` (we commit a small one for the
  Greek NT vs KJV pair so the notebook can be rebuilt without
  re-embedding everything).
