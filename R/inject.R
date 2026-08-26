# Post-render step: rewrite the reference links altdoc/Quarto/downlit already
# produced so each one carries a doxygen-style hover tooltip. This is a plain
# HTML rewrite over the finished build output, the same strategy
# DocumenterCodeBlocks.jl uses (see its pipeline.jl) -- it doesn't require
# hooking into whichever tool (Quarto, mkdocs, docsify, docute) rendered the
# site, only that the rendered HTML is on disk.
#
# What we match: Pandoc's syntax highlighter (skylighting) tags a called
# function's identifier as `<span class="fu">`, and downlit wraps recognized
# ones in `<a href="...">`. That combination -- `<span class="fu"><a
# href="...">name</a></span>` -- is exactly a call-site reference link,
# whether it resolves locally (docs/man/*.html) or, for a local package
# without a live pkgdown.yml, out to rdrr.io (see altdoc issue #68). Either
# way the *name* is enough to look up the local package's own
# signature/brief, so both cases get a tooltip.

.FU_LINK_RE <- "<span class=\"fu\"><a href=\"([^\"]*)\">([^<]*)</a></span>"

#' Add hover tooltips to an altdoc site's reference links
#'
#' Runs after `altdoc::render_docs()`. Reads the package's own `man/*.Rd`
#' files to build a name -> signature/summary index (see
#' [build_topic_index()]), then rewrites every `docs/**/*.html` file,
#' attaching a doxygen-style hover tooltip to each function-call reference
#' link already produced by Quarto's `code-link`/downlit. Links that don't
#' name a documented object in this package are left untouched.
#'
#' Idempotent: a file that already carries reftip's marker comment is left
#' as-is (rerun after a fresh `render_docs()`, not after `add_tooltips()`
#' itself).
#'
#' @param path Path to the package root.
#' @param docs_dir Path to the built site. Defaults to `file.path(path,
#'   "docs")`, which is where `altdoc::render_docs()` always leaves the
#'   final HTML for all four backends (quarto_website, mkdocs, docsify,
#'   docute).
#' @param quiet Logical. Suppress progress messages.
#' @return Invisibly, a list with `files` (how many HTML files were touched)
#'   and `links` (how many reference links got a tooltip).
#' @export
add_tooltips <- function(path = ".", docs_dir = NULL, quiet = FALSE) {
    if (is.null(docs_dir)) {
        docs_dir <- fs::path_join(c(path, "docs"))
    }
    if (!fs::dir_exists(docs_dir)) {
        stop(sprintf(
            "No built site found at '%s'. Run altdoc::render_docs() first.",
            docs_dir
        ), call. = FALSE)
    }

    index <- build_topic_index(path)
    if (length(index) == 0) {
        if (!quiet) message("reftip: no documented topics found in man/*.Rd; nothing to do.")
        return(invisible(list(files = 0L, links = 0L)))
    }

    html_files <- fs::dir_ls(docs_dir, regexp = "\\.html$", recurse = TRUE)
    n_files <- 0L
    n_links <- 0L

    for (f in html_files) {
        html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        if (grepl(.REFTIP_MARKER, html, fixed = TRUE)) next   # already processed

        result <- .inject_tooltips_html(html, index)
        if (result$n == 0) next

        new_html <- result$html
        if (grepl("</head>", new_html, fixed = TRUE)) {
            new_html <- sub("</head>", paste0(.reftip_css_block(), "</head>"), new_html, fixed = TRUE)
        }
        body_addition <- paste0(.reftip_payload_html(result$tips), .reftip_script_block())
        if (grepl("</body>", new_html, fixed = TRUE)) {
            new_html <- sub("</body>", paste0(body_addition, "</body>"), new_html, fixed = TRUE)
        } else {
            new_html <- paste0(new_html, body_addition)
        }

        writeLines(new_html, f, useBytes = TRUE)
        n_files <- n_files + 1L
        n_links <- n_links + result$n
    }

    if (!quiet) {
        message(sprintf(
            "reftip: added tooltips to %d reference link%s across %d file%s.",
            n_links, if (n_links == 1) "" else "s",
            n_files, if (n_files == 1) "" else "s"
        ))
    }

    invisible(list(files = n_files, links = n_links))
}

# Single pass over one HTML document: every `.FU_LINK_RE` match whose name
# resolves in `index` gets its `<a>` tagged with `class="reftip-ref"` and
# `data-reftip="<id>"`; everything else (including unresolved reference
# links) is copied through unchanged. `tips` collects one entry per unique
# resolved name used on the page (id -> {usage, brief}), in first-appearance
# order -- the page's hidden tooltip payload. Ids rather than raw names
# avoid any HTML-attribute-escaping concerns for the (rare) operator-like R
# names.
#
# Uses `gregexpr(..., perl = TRUE)` capture positions directly instead of a
# second `regexec()` pass per match.
.inject_tooltips_html <- function(html, index) {
    m <- gregexpr(.FU_LINK_RE, html, perl = TRUE)[[1]]
    if (m[1] == -1) {
        return(list(html = html, n = 0L, tips = list()))
    }

    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    cap_starts <- attr(m, "capture.start")
    cap_lens <- attr(m, "capture.length")

    pieces <- vector("list", length(starts) * 2L + 1L)
    pos <- 1L
    pi <- 1L
    n_linked <- 0L
    tip_ids <- character(0)   # name -> id, assigned on first appearance
    tips <- list()            # id -> entry

    for (i in seq_along(starts)) {
        s <- starts[i]
        l <- lens[i]
        href <- substr(html, cap_starts[i, 1], cap_starts[i, 1] + cap_lens[i, 1] - 1L)
        name_html <- substr(html, cap_starts[i, 2], cap_starts[i, 2] + cap_lens[i, 2] - 1L)
        name <- .html_unescape(name_html)

        entry <- index[[name]]

        pieces[[pi]] <- substr(html, pos, s - 1L)
        pi <- pi + 1L
        if (is.null(entry)) {
            pieces[[pi]] <- substr(html, s, s + l - 1L)
        } else {
            id <- tip_ids[name]
            if (is.na(id)) {
                id <- paste0("t", length(tips) + 1L)
                tip_ids[name] <- id
                tips[[id]] <- entry
            }
            pieces[[pi]] <- sprintf(
                '<span class="fu"><a class="reftip-ref" data-reftip="%s" href="%s">%s</a></span>',
                id, href, name_html
            )
            n_linked <- n_linked + 1L
        }
        pi <- pi + 1L
        pos <- s + l
    }
    pieces[[pi]] <- substr(html, pos, nchar(html))

    list(html = paste(unlist(pieces), collapse = ""), n = n_linked, tips = tips)
}
