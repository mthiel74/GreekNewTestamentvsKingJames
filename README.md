# Greek New Testament vs King James — translation-pair framework

Sentence-by-sentence alignment of the **Koine Greek New Testament**
(SBLGNT) and the **King James Version** (1769 Cambridge), with
multilingual embeddings, semantic-distance ranking, and a frontier
LLM that hypothesises *why* each large gap exists — theological,
philological, archaic English, manuscript variant.

The framework is the staff-pick, not the Bible alone. The same
pipeline runs on any aligned text pair:

| pair                                          | what it stresses                                |
| --------------------------------------------- | ----------------------------------------------- |
| **Greek NT (SBLGNT) vs KJV**                  | Koine → 17th-c. English; theological loading   |
| KJV vs Luther 1545                            | English vs German; cognate but lexically apart  |
| KJV vs Reina-Valera 1909                      | English vs Spanish; Romance pivot               |
| Iliad: Pope (1720) vs Butler (1898) prose     | verse → prose; same source language             |
| Marx, *Kapital* I: German vs Moore-Aveling    | technical / philosophical translation           |
| Tao Te Ching: Legge (1891) vs Goddard (1919)  | classical Chinese → English, two Victorians     |

All texts used are either **public domain** or under a permissive
licence (SBLGNT is CC BY 4.0). See `docs/sources.md` for canonical
URLs, retrieval dates, and per-source licence notes.

## What the project does

1. **Fetches** verse-keyed plain text from canonical open repositories
   (`morphgnt/sblgnt` for the Greek NT, `aruljohn/Bible-kjv` for the
   KJV, Project Gutenberg / Wikisource for the rest).
2. **Aligns** the two sides at verse granularity. Bible-shaped pairs
   inherit the canonical Book / Chapter / Verse axis; non-Bible pairs
   use a sentence-level aligner (paragraph-anchored DP over length
   ratios — see `wolfram/framework.wl`).
3. **Embeds** both sides with a single multilingual model
   (Cohere `embed-multilingual-v3.0` by default; OpenAI
   `text-embedding-3-large` is a drop-in alternative). The same model
   embeds Greek, English, German, Spanish and Chinese in a shared
   space, so cosine distance is meaningful across languages.
4. **Ranks** each aligned unit by **cosine distance** between the two
   embeddings. Low distance = the translation tracks the source.
   High distance = the translation has moved.
5. **Asks a frontier LLM** (Claude or GPT) to hypothesise *why* each
   of the top-N divergent verses diverged. The LLM is given:
   the source verse, the translation, the cosine distance, and a
   short rubric (theological loading? archaic vocabulary?
   manuscript variant? translation strategy? etc.) and returns a
   structured JSON tag + a one-paragraph explanation.
6. Produces a self-contained **Wolfram Community notebook** in
   `community/` (`.wls` source + committed `.nb` and `.pdf`).

## Repository layout

The entire analysis pipeline is **pure Wolfram Language**. No Python
is required to rebuild figures or the notebook.

| path                                | what lives there                                                |
| ----------------------------------- | --------------------------------------------------------------- |
| `wolfram/fetcher_common.wl`         | shared HTTP / cache helpers                                     |
| `wolfram/fetch_kjv.wls`             | downloads the 27 NT books of the KJV as verse-keyed JSON        |
| `wolfram/fetch_greek_nt.wls`        | downloads SBLGNT morphgnt files and reconstructs verse text     |
| `wolfram/fetch_extra_pairs.wls`     | optional: Luther 1545, Reina-Valera, Iliad, Tao Te Ching, Marx  |
| `wolfram/framework.wl`              | alignment, embedding, distance, LLM hypothesis-generation       |
| `wolfram/embed.wls`                 | regenerates the embedding cache for a configured pair           |
| `wolfram/analyze.wls`               | ranks the top-N divergent units and produces the LLM analysis   |
| `wolfram/run_all.wls`               | one-shot driver: fetch → embed → distance → analyse → figures   |
| `data/*.json`                       | verse-keyed text (committed; small, regenerable)                |
| `data/raw/`                         | bulk raw downloads (git-ignored)                                |
| `data/embeddings/`                  | binary embedding caches (git-ignored)                           |
| `community/build_notebook.wls`      | assembles the Wolfram Community notebook                        |
| `community/*.nb`, `community/*.pdf` | committed outputs of the build script                           |
| `docs/sources.md`                   | canonical URL, retrieval date, licence per source               |
| `docs/api_keys.md`                  | which env vars are needed and how to set them                   |
| `docs/images/`                      | figures referenced from the notebook                            |
| `tests/`                            | sanity checks: verse counts, alignment shape, distance bounds   |

## Reproducing

You need:

* Wolfram Engine ≥ 14.0 (or Mathematica)
* `wolframscript` on `PATH`
* one of: `COHERE_API_KEY`, `OPENAI_API_KEY`, or `VOYAGE_API_KEY`
  (for embeddings)
* one of: `ANTHROPIC_API_KEY`, `OPENAI_API_KEY` (for the
  "why is this verse divergent" LLM pass)

Then:

```sh
# 1. Fetch verse-keyed text for the headline pair (Greek NT + KJV)
wolframscript -file wolfram/fetch_greek_nt.wls
wolframscript -file wolfram/fetch_kjv.wls

# 2. Compute embeddings (writes data/embeddings/<pair>.mx)
wolframscript -file wolfram/embed.wls -- greek-nt kjv

# 3. Rank divergent verses + ask the LLM why
wolframscript -file wolfram/analyze.wls -- greek-nt kjv

# 4. Build the Wolfram Community notebook
wolframscript -file community/build_notebook.wls
```

`wolfram/run_all.wls` chains all four for you.

## Why this is interesting

Translation-divergence ranking gives a *quantitative entry point*
to questions usually framed qualitatively:

* **Where does the KJV's 1611 English drift hardest from its Koine
  source?** Mostly: rhetorical pronouns, theologically loaded nouns,
  and idioms with no English cognate (e.g. σπλάγχνα, "bowels of
  compassion", *Phil 1:8*).
* **Which divergences are textual, not stylistic?** The Comma
  Johanneum (*1 John 5:7-8*) and the longer ending of Mark have
  unusually large cosine gaps because the KJV translates a Greek
  base text the SBLGNT does not contain.
* **Does the same framework recover known facts about other
  text pairs?** Pope's Iliad rhyming couplets diverge from Butler's
  prose mostly on poetic compression; Moore-Aveling Marx diverges
  from the German on technical vocabulary that didn't exist in
  English yet.

The LLM's hypothesis tags are *not* ground truth. They are a fast
way to read a 50-row table of divergent verses and form a hypothesis
worth checking against a commentary. The Wolfram Community post
makes this transparent.

## Status

Skeleton in place. Headline figure (cosine-distance heatmap across
the 27 NT books) is the first concrete deliverable.

## Related projects

* [ENSO-emergence](https://github.com/mthiel74/ENSO-emergence) — the
  notebook-build pattern this repo follows
* [Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms)
