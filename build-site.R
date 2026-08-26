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
# altdoc/demo/demo.R and altdoc/demo/demo.qmd). Those are staged into man/
# and vignettes/ only for the duration of the build, then removed, so
# nothing about them ships in the package or lingers in the working tree.

pkg_path <- "."

# Stage/unstage the demo API

stage_demo <- function(path = pkg_path) {
  demo_src <- readLines(file.path(path, "altdoc", "demo", "demo.R"), warn = FALSE)
  rd <- roxygen2::roc_proc_text(roxygen2::rd_roclet(), paste(demo_src, collapse = "\n"))

  man_dir <- file.path(path, "man")
  rd_paths <- file.path(man_dir, names(rd))
  for (nm in names(rd)) {
    writeLines(format(rd[[nm]]), file.path(man_dir, nm))
  }

  vig_dir <- file.path(path, "vignettes")
  vig_existed <- dir.exists(vig_dir)
  if (!vig_existed) dir.create(vig_dir)
  qmd_path <- file.path(vig_dir, "demo.qmd")
  file.copy(file.path(path, "altdoc", "demo", "demo.qmd"), qmd_path, overwrite = TRUE)

  list(rd_paths = rd_paths, qmd_path = qmd_path, vig_dir = vig_dir, vig_existed = vig_existed)
}

unstage_demo <- function(staged) {
  unlink(staged$rd_paths)
  unlink(staged$qmd_path)
  if (!staged$vig_existed && length(list.files(staged$vig_dir)) == 0) {
    unlink(staged$vig_dir, recursive = TRUE)
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
