#' Build a reference index from a package's Rd files
#'
#' Parses every `man/*.Rd` file under `path` and returns, for each documented
#' alias, its usage, a one-line summary, and the topic it's documented on.
#'
#' When an Rd page documents several aliases (e.g. an S3 generic and its
#' methods), its `\\usage{}` block is split on blank lines and each alias is
#' matched to the block whose call starts with that alias's name. An alias
#' with no matching block falls back to the whole `\\usage{}` text, with a
#' warning.
#'
#' @param path Path to the package root (must contain a `man/` directory).
#' @param quiet Logical. Suppress the warnings issued when a `\\usage{}` block
#'   couldn't be narrowed to one alias, or when a tooltip brief had to be
#'   truncated mid-word.
#' @return A named list, one entry per alias, each holding `usage`, `brief`
#'   (character or `NULL`), and `topic`.
#' @export
build_topic_index <- function(path = ".", quiet = FALSE) {
    man_dir <- fs::path_join(c(path, "man"))
    if (!fs::dir_exists(man_dir)) {
        stop(sprintf("No 'man/' directory found at '%s'.", path), call. = FALSE)
    }

    rd_files <- fs::dir_ls(man_dir, regexp = "\\.Rd$")
    index <- list()

    for (rd_file in rd_files) {
        rd <- tryCatch(tools::parse_Rd(rd_file), error = function(e) NULL)
        if (is.null(rd)) next
        topic <- fs::path_ext_remove(fs::path_file(rd_file))

        tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
        aliases <- vapply(
            rd[tags == "\\alias"],
            function(x) as.character(x[[1]]),
            character(1)
        )
        if (length(aliases) == 0) next

        usage_node <- .rd_usage_node(rd, tags)
        usage_blocks <- if (is.null(usage_node)) character(0) else .rd_usage_blocks(usage_node)
        usage_whole <- if (length(usage_blocks) == 0) NULL else paste(usage_blocks, collapse = "\n\n")
        usage_by_name <- usage_blocks
        names(usage_by_name) <- vapply(usage_blocks, .rd_usage_leading_name, character(1))

        description <- .rd_section_text(rd, tags, "\\description")
        sentence <- .first_sentence(description)
        brief <- sentence$text

        unmatched <- setdiff(aliases, names(usage_by_name))
        if (!quiet && length(aliases) > 1 && length(unmatched) > 0) {
            warning(sprintf(
                "reftip: '%s' documents %d aliases but couldn't match a \\usage{} line to %s; showing the full shared \\usage{} block for %s instead of just its own signature.",
                basename(rd_file), length(aliases),
                paste(sprintf("'%s'", unmatched), collapse = ", "),
                if (length(unmatched) == 1) "it" else "them"
            ), call. = FALSE)
        }
        if (!quiet && sentence$clipped) {
            warning(sprintf(
                "reftip: the tooltip brief for '%s' is longer than 200 characters and got truncated mid-word. Shorten the first sentence of \\description{} to fit within 200 characters.",
                basename(rd_file)
            ), call. = FALSE)
        }

        for (alias in aliases) {
            match_idx <- match(alias, names(usage_by_name))
            usage <- if (is.na(match_idx)) usage_whole else usage_by_name[[match_idx]]
            index[[alias]] <- list(usage = usage, brief = brief, topic = topic)
        }
    }

    index
}

# Raw \usage{} Rd node for one Rd file, or NULL if absent.
.rd_usage_node <- function(rd, tags) {
    hit <- rd[tags == "\\usage"]
    if (length(hit) == 0) NULL else hit[[1]]
}

# Split a \usage{} node into one block per signature, on blank-line
# separators. A \method{generic}{class} macro is reassembled as
# "generic.class", matching the S3 method's alias spelling (e.g. "print.foo").
.rd_usage_blocks <- function(usage_node) {
    blocks <- character(0)
    current <- character(0)

    flush <- function() {
        if (length(current) > 0) {
            text <- trimws(paste(current, collapse = ""))
            if (nchar(text) > 0) blocks[[length(blocks) + 1L]] <<- text
        }
        current <<- character(0)
    }

    for (child in usage_node) {
        if (identical(attr(child, "Rd_tag"), "\\method")) {
            current[[length(current) + 1L]] <- paste0(
                paste(unlist(child[[1]]), collapse = ""), ".",
                paste(unlist(child[[2]]), collapse = "")
            )
        } else {
            piece <- paste(unlist(child), collapse = "")
            if (nchar(trimws(piece)) == 0) {
                flush()   # a blank-only run separates one signature from the next
            } else {
                current[[length(current) + 1L]] <- piece
            }
        }
    }
    flush()

    blocks
}

# The alias a \usage{} block documents: the identifier before its first "(".
.rd_usage_leading_name <- function(block) {
    name <- trimws(sub("\\(.*$", "", block, perl = TRUE))
    sub("^`(.*)`$", "\\1", name)
}

# Plain-text content of a top-level Rd section (e.g. "\\usage"), stripped of
# markup. NULL when the section is absent.
.rd_section_text <- function(rd, tags, section) {
    hit <- rd[tags == section]
    if (length(hit) == 0) {
        return(NULL)
    }
    text <- paste(unlist(hit), collapse = "")
    text <- trimws(text)
    if (nchar(text) == 0) NULL else text
}
