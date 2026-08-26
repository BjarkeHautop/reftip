# reftip

Hover tooltips for the function/reference links on an
[altdoc](https://github.com/etiennebacher/altdoc) site. Inspired by
[DocumenterCodeBlocks.jl](https://fredrikekre.github.io/DocumenterCodeBlocks.jl/dev/).

altdoc's `quarto_website` backend already turns function names in R code
into links, via Quarto's `code-link` and the
[downlit](https://downlit.r-lib.org/) package. But downlit can't link a
local dev version or unpublished package, and it doesn't work on stuff
like an S3 method `print.animal()`.

reftip fixes that. Run it once after `altdoc::render_docs()`:

``` r
altdoc::render_docs()
reftip::add_tooltips()
```

It only touches references to the package's own topics; anything else
(base R, another package, code it can't resolve at all) is left exactly
as Quarto/downlit rendered it. It targets the `quarto_website` backend
specifically; the other three (mkdocs, docsify, docute) haven't been
tried.

See it in action in [live demo](/vignettes/demo.html).

## Known limitations

- A generic call like `print(x)` isn't resolved, since that needs
  knowing `x`'s class at runtime, and reftip only reads `man/*.Rd`. It
  doesn't evaluate any code.
- Same reason `obj$method()` (R6) isn't resolved: nothing says which
  class's methods to look up.
- An Rd page with several `\alias{}`es sharing one `\usage{}` block (an
  S3 generic plus its methods) shows that whole block in the tooltip,
  not just the one alias's line.
