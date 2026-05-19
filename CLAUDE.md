# Repo notes for Claude

## Purpose

A Wolfram Community post that builds a **translation-pair divergence
framework** — pick two translations of the same source text,
sentence-align them, embed both sides with a multilingual model,
rank verses (or sentences) by semantic distance, then ask a frontier
LLM *why* each large gap exists.

The headline case study is the **Koine Greek New Testament (SBLGNT)
vs the King James Version (1611/1769 Cambridge)**. The framework
then generalises to:

* KJV vs Luther 1545 (English vs German Bible)
* KJV vs Reina-Valera 1909 (English vs Spanish Bible)
* Iliad: Pope (1715-20) vs the public-domain Butler (1898) prose
* Marx, *Das Kapital* vol. I, German original vs Moore-Aveling (1887)
* Tao Te Ching: Legge (1891) vs Goddard-Borel (1919)

## Pipeline (pure Wolfram Language)

```
wolfram/fetcher_common.wl        shared HTTP / cache helpers
wolfram/fetch_kjv.wls            ──┐
wolfram/fetch_greek_nt.wls         ├─> data/*.json (verse-keyed)
wolfram/fetch_extra_pairs.wls    ──┘

wolfram/framework.wl             core: alignment, embedding, distance,
                                 LLM hypothesis generation
wolfram/embed.wls                regenerates the embedding cache for
                                 a configured text pair
wolfram/analyze.wls              ranks gaps + writes the LLM analysis
wolfram/run_all.wls              one-shot driver: fetch → embed →
                                 distance → analyse → figures

community/build_notebook.wls     assembles the long-form .nb
```

## Conventions

* Plain `.wls` / `.wl` is the source of truth. The `.nb` and `.pdf`
  in `community/` are committed *outputs*.
* All HTTP fetches go through `URLRead[HTTPRequest[...]]` and check
  the status code — never `URLDownload` (it silently writes error
  pages on 4xx/5xx).
* Verse-keyed JSON files in `data/` are committed; raw downloads
  and embedding caches in `data/raw/` and `data/embeddings/` are
  git-ignored.
* API keys for embedding + LLM providers come from environment
  variables (`COHERE_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`),
  never from committed files. See `docs/api_keys.md`.
* Costs: a full SBLGNT-vs-KJV embedding pass with Cohere
  `embed-multilingual-v3.0` is ≈16 k input strings ≈ a few cents.
  The LLM "why" pass scales with how many top-N divergent verses
  you analyse (default 50).

## Licensing of source texts

* **SBLGNT** (Greek NT, Holmes 2010) — CC BY 4.0
  via the `morphgnt/sblgnt` repo. Attribution requirement: cite
  Michael W. Holmes (ed.), *The SBL Greek New Testament*, 2010, and
  reference the SBLGNT licence.
* **KJV** — public domain (US); Cambridge 1769 edition via the
  `aruljohn/Bible-kjv` repo.
* **Luther 1545** — public domain.
* **Reina-Valera 1909** — public domain.
* **Pope's Iliad** / **Butler's Iliad** / **Legge's Tao Te Ching** /
  **Goddard's Tao Te Ching** / **Moore-Aveling Kapital** — all
  public domain via Project Gutenberg / Wikisource.

`docs/sources.md` keeps the canonical URL, retrieval date, and
licence note per source.

## Commit cadence

Commit + push after each meaningful step: scaffold, fetchers,
framework, first divergence figure, secondary pair, notebook
section. Short, factual messages.
