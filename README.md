# reftip

Hover tooltips for the function/reference links on an
[pkgdown](https://pkgdown.r-lib.org/) or
[altdoc](https://github.com/etiennebacher/altdoc) site. Inspired by
[DocumenterCodeBlocks.jl](https://fredrikekre.github.io/DocumenterCodeBlocks.jl/dev/).

Build your docs like normal via `pkgdown::build_site()` or `altdoc::render_docs()`
and then run:

``` r
reftip::add_tooltips()
```

It auto-detects which of the two built the site; pass `site = "altdoc"` or
`site = "pkgdown"` explicitly if detection is ambiguous. Either way the tooltip, only
touches references to the package's own topics; anything else (base R,
another package, code it can't resolve) is left alone.

See it in action in [live demo](https://bjarkehautop.github.io/reftip/vignettes/demo.html).

## Installation

Install from GitHub:

``` r
pak:pak("https://github.com/BjarkeHautop/reftip")
```

## Build locally

Both site types can be built from this repo using a couple of throwaway
demo functions (see `demo/demo.R`) that exist only to show off the
tooltips:

``` r
pkgload::load_all()

# altdoc
source("altdoc/build-site.R")
build_site()

# pkgdown
source("pkgdown/build-site.R")
build_site()
```

Open `docs/index.html` (altdoc) or `docs-pkgdown/index.html` (pkgdown)
afterward and hover over functions to see it in action.

## What's in the tooltip

- The matched `\usage{}` line,
- The first sentence of `\description{}`.

If the description's first sentence runs past 200 characters `add_tooltips()` warns, naming the Rd file, so you know to
shorten it. Pass `quiet = TRUE` to suppress.

## Known limitations

- A generic call like `print(x)` isn't resolved, since that needs
  knowing `x`'s class at runtime, and reftip only reads `man/*.Rd`. It
  doesn't evaluate any code.
- Same reason `obj$method()` (R6) isn't resolved: nothing says which
  class's methods to look up.
- An Rd page with several `\alias{}`es sharing one `\usage{}` block (an
  S3 generic plus its methods) shows that whole block in the tooltip,
  not just the one alias's line.
