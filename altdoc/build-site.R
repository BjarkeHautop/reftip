# Build script for reftip's own altdoc site.
#
# Usage:
#   Rscript altdoc/build-site.R
# or, from an R session (package already loaded, e.g. via pkgload::load_all()):
#   source("altdoc/build-site.R")
#   build_site()
#
# Unlike a package documenting a real public API, reftip's own site needs a
# few fake, documented functions purely to show off its hover tooltips (see
# demo/demo.R, demo/demo.qmd, and demo/demo.Rmd). Those are staged into
# man/ and vignettes/ only for the duration of the build, then removed, so
# nothing about them ships in the package or lingers in the working tree.
# demo/ lives at the repo root, outside altdoc/ and pkgdown/, since altdoc
# mirrors the whole altdoc/ tree into its own Quarto project -- a second
# demo.* file living there would collide with demo.qmd as a render target.

pkg_path <- "."

# Stage/unstage the demo API. `vignette` picks which narrative file to
# stage as vignettes/demo.<ext>: "qmd" (altdoc's own Quarto backend), "rmd"
# (pkgdown, via rmarkdown), or "none". Both narratives have the same
# content -- Pandoc/knitr syntax-highlights their code blocks the same way
# either backend renders them, so downlit (and reftip after it) can link
# and tag them identically.

stage_demo <- function(path = pkg_path, vignette = c("qmd", "rmd", "none")) {
  vignette <- match.arg(vignette)

  demo_src <- readLines(file.path(path, "demo", "demo.R"), warn = FALSE)
  rd <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), paste(demo_src, collapse = "\n"))

  man_dir <- file.path(path, "man")
  rd_paths <- file.path(man_dir, names(rd))
  for (nm in names(rd)) {
    writeLines(format(rd[[nm]]), file.path(man_dir, nm))
  }

  vig_path <- NULL
  vig_dir <- NULL
  vig_existed <- NA
  if (vignette != "none") {
    # pkgdown expects the conventional "Rmd" case; altdoc's Quarto backend
    # expects lowercase "qmd".
    filename <- paste0("demo.", c(qmd = "qmd", rmd = "Rmd")[[vignette]])
    src_path <- file.path(path, "demo", filename)

    vig_dir <- file.path(path, "vignettes")
    vig_existed <- dir.exists(vig_dir)
    if (!vig_existed) dir.create(vig_dir)
    vig_path <- file.path(vig_dir, filename)
    if (!file.copy(src_path, vig_path, overwrite = TRUE)) {
      stop(sprintf("Failed to stage demo vignette from '%s' to '%s'.", src_path, vig_path), call. = FALSE)
    }
  }

  list(rd_paths = rd_paths, vig_path = vig_path, vig_dir = vig_dir, vig_existed = vig_existed)
}

unstage_demo <- function(staged) {
  unlink(staged$rd_paths)
  if (!is.null(staged$vig_path)) {
    unlink(staged$vig_path)
    if (!staged$vig_existed && length(list.files(staged$vig_dir)) == 0) {
      unlink(staged$vig_dir, recursive = TRUE)
    }
  }
}

# Entry point

build_site <- function(path = pkg_path, ...) {
  staged <- stage_demo(path)
  on.exit(unstage_demo(staged), add = TRUE)

  altdoc::render_docs(path = path, ...)
  reftip::add_tooltips(path = path)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  build_site()
}
