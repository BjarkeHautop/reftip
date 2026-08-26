# Build a name -> {usage, brief} index straight from the package's man/*.Rd
# files. This mirrors what altdoc's own `.rd_topic_index()` does (see
# R/reference_index.R in altdoc) but keeps the \usage and \description
# content instead of just the \title -- that's the signature + summary a
# doxygen-style tooltip needs.

#' Build a reference index from a package's Rd files
#'
#' Parses every `man/*.Rd` file under `path` and returns, for each documented
#' alias, its usage (signature) and a one-line summary taken from the first
#' sentence of its description. This is the same information downlit's
#' generated links point at -- reftip just re-extracts it locally so it can
#' be embedded in the tooltip at build time, without any network access.
#'
#' @param path Path to the package root (must contain a `man/` directory).
#' @return A named list, one entry per alias, each holding `usage` (character,
#'   the raw `\\usage{}` text) and `brief` (character or `NULL`, the first
#'   sentence of `\\description{}`).
#' @export
build_topic_index <- function(path = ".") {
    man_dir <- fs::path_join(c(path, "man"))
    if (!fs::dir_exists(man_dir)) {
        stop(sprintf("No 'man/' directory found at '%s'.", path), call. = FALSE)
    }

    rd_files <- fs::dir_ls(man_dir, regexp = "\\.Rd$")
    index <- list()

    for (rd_file in rd_files) {
        rd <- tryCatch(tools::parse_Rd(rd_file), error = function(e) NULL)
        if (is.null(rd)) next

        tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
        aliases <- vapply(
            rd[tags == "\\alias"],
            function(x) as.character(x[[1]]),
            character(1)
        )
        if (length(aliases) == 0) next

        usage <- .rd_section_text(rd, tags, "\\usage")
        description <- .rd_section_text(rd, tags, "\\description")
        brief <- .first_sentence(description)

        entry <- list(usage = usage, brief = brief)
        for (alias in aliases) {
            index[[alias]] <- entry
        }
    }

    index
}

# The plain-text content of a top-level Rd section (e.g. "\\usage" or
# "\\description"), flattened the same way altdoc's `.rd_topic_index()`
# flattens `\\title` -- `unlist()` walks nested Rd fragments (macros like
# `\\code{}`/`\\link{}`) into their literal text runs in order, which loses
# markup but keeps the wording and spacing intact. `NULL` when the section
# is absent.
.rd_section_text <- function(rd, tags, section) {
    hit <- rd[tags == section]
    if (length(hit) == 0) {
        return(NULL)
    }
    text <- paste(unlist(hit), collapse = "")
    text <- trimws(text)
    if (nchar(text) == 0) NULL else text
}
