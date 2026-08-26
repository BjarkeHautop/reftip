# reftip

Doxygen-style hover tooltips for the reference links in an [altdoc](https://github.com/etiennebacher/altdoc) site.

## Why

altdoc's `quarto_website` backend already turns function names in rendered R
code into links to their documentation, via Quarto's `code-link` option and
the [downlit](https://downlit.r-lib.org/) package
([altdoc#68](https://github.com/etiennebacher/altdoc/issues/68)). What it
doesn't do is show anything *at* the link: no signature, no summary, nothing
until you click through.

reftip adds that. Run it once after `altdoc::render_docs()`:

```r
altdoc::render_docs()
reftip::add_tooltips()
```

Every reference link downlit produced now shows a floating tooltip on hover
(or keyboard focus) with the target's signature (from its Rd `\usage{}`) and
a one-sentence summary (the first sentence of its Rd `\description{}`),
embedded at build time -- no network access needed to see it, even for links
that point out to rdrr.io.

## What it is (and isn't)

- A **separate package**, not a change to altdoc. It's a post-processing
  pass over the finished `docs/**/*.html` altdoc already built.
- **Tooltips only.** The reference *linking* itself is already handled by
  Quarto/downlit; reftip doesn't duplicate that logic, it just annotates the
  links that are already there.
- Built by reading the package's own `man/*.Rd` files directly -- it doesn't
  parse or evaluate the rendered R code, and it doesn't need Documenter.jl's
  kind of live build-time object graph (R doesn't have one).

## How it works

1. `build_topic_index()` parses `man/*.Rd` into a `name -> {usage, brief}`
   index.
2. `add_tooltips()` walks the built HTML and finds
   `<span class="fu"><a href="...">name</a></span>` -- Pandoc's syntax
   highlighter tags a called function's identifier `fu`, and that's exactly
   what downlit wraps in a link. Every one whose name is in the index gets
   tagged `class="reftip-ref" data-reftip="<id>"`.
3. One hidden payload div (`.reftip-tip[data-for="<id>"]`) per unique
   reference used on the page, plus a small vanilla-JS snippet, are appended
   before `</body>`. The script shows a `position: fixed` tooltip on
   hover/focus, positioned from `getBoundingClientRect()`.

`position: fixed` (rather than a pure-CSS `:hover` sibling) matters: Quarto's
own stylesheet sets `overflow: auto` on the code-block wrapper so long lines
scroll horizontally, and that clips anything absolutely positioned inside it.
Fixed positioning is relative to the viewport, so it escapes that clipping
regardless of which of altdoc's four backends rendered the page.

## Status

Early prototype. Confirmed working against `quarto_website` (vignettes and
man pages, including horizontally-scrollable code blocks). Known
limitations:

- Only call-site references (`class="fu"`) are tagged -- bare value mentions
  aren't, matching downlit's own linking behavior.
- A page-level Rd file with several `\alias{}`es (e.g. an S3 generic plus its
  methods) shares one `\usage{}` block across all of them; the tooltip for
  any one alias shows the whole block, not just its own line.
- Not yet tried against mkdocs/docsify/docute output.

## Development

`dev/fixturepkg/` is a minimal R package used to validate reftip against a
real altdoc + Quarto build:

```r
devtools::install("dev/fixturepkg")
altdoc::setup_docs(path = "dev/fixturepkg", tool = "quarto_website")
altdoc::render_docs(path = "dev/fixturepkg")
devtools::load_all(".")
add_tooltips(path = "dev/fixturepkg")
```
