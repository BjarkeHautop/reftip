# Fake API used only to showcase reftip's tooltips on its own documentation
# site (see altdoc/demo/demo.qmd). altdoc/build-site.R stages this file's
# generated man pages into man/, and demo.qmd into vignettes/, only for the
# duration of `altdoc::render_docs()`, then removes both, so nothing here
# ships in the package or lingers in the working tree.

#' Greet someone
#'
#' Prints a friendly greeting.
#'
#' @param name Character. Who to greet.
#' @return Invisibly, the greeting string.
#' @examples
#' \dontrun{
#' greet("world")
#' }
#' @export
greet <- function(name = "world") {
  msg <- paste0("Hello, ", name, "!")
  print(msg)
  invisible(msg)
}

#' An animal
#'
#' A minimal S3 class. Exists only to demo the case downlit can't resolve on
#' its own: a call to an S3 method written out by its full dotted name.
#'
#' @param name Character. The animal's name.
#' @param sound Character. The sound it makes.
#' @return An object of class `animal`.
#' @examples
#' \dontrun{
#' rex <- animal("Rex", "woof")
#' print.animal(rex)
#' }
#' @export
animal <- function(name, sound) {
  structure(list(name = name, sound = sound), class = "animal")
}

#' @rdname animal
#' @param x An `animal` object.
#' @param ... Unused.
#' @export
print.animal <- function(x, ...) {
  cat(sprintf("%s says %s!\n", x$name, x$sound))
  invisible(x)
}
