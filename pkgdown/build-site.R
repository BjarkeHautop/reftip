# Build script for reftip's own pkgdown site, used to demo reftip's
# pkgdown support alongside its altdoc one (see altdoc/build-site.R).
# Builds to docs-pkgdown/ rather than docs/ (altdoc's own site) so the two
# can coexist and reftip's site-type auto-detection isn't left looking at
# a directory with both a man/ and a reference/ subdirectory.
#
# Usage:
#   Rscript pkgdown/build-site.R
# or, from an R session (package already loaded, e.g. via pkgload::load_all()):
#   source("pkgdown/build-site.R")
#   build_site()
#
# Reuses the same fake demo API as the altdoc build (see demo/demo.R),
# staged into man/ only for the duration of the build, then removed. The
# demo narrative itself is staged from demo/demo.Rmd rather than
# demo/demo.qmd (pkgdown renders articles via rmarkdown, not Quarto) --
# same content, so the same tooltips show up.

pkg_path <- "."
.altdoc_env <- new.env()
source(file.path(pkg_path, "altdoc", "build-site.R"), local = .altdoc_env)
stage_demo <- .altdoc_env$stage_demo
unstage_demo <- .altdoc_env$unstage_demo

build_site <- function(path = pkg_path, destination = file.path(path, "docs-pkgdown"), ...) {
  staged <- stage_demo(path, vignette = "rmd")
  on.exit(unstage_demo(staged), add = TRUE)

  pkgdown::build_site(pkg = path, override = list(destination = destination), preview = FALSE, ...)
  reftip::add_tooltips(path = path, docs_dir = destination)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  build_site()
}
