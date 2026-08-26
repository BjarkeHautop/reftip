# Rewrites Quarto's rendered HTML so references to this package's own
# documented topics link straight to docs/man/<topic>.html and get a hover
# tooltip, instead of relying on downlit (which can't resolve a local/
# unpublished package, or a name it can't statically dispatch, e.g. an S3
# method called by name).
#
# Two reference shapes are matched, each optionally already wrapped in a
# downlit link:
#  1. A called function inside a fenced code block: Pandoc tags it
#     `<span class="fu">name</span>` (or "va" for a referenced-but-not-
#     called value, e.g. an R6 class name).
#  2. Inline code in prose (`` `foo()` ``): `<code>name()</code>`.
# Both may already carry an `<a href="...">` from downlit; it's discarded
# when the name is one of ours, kept otherwise.

.FU_LINK_RE <- paste0(
    "<span class=\"(fu|va)\">(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</span>",
    "|",
    "<code>(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</code>"
)

#' Add hover tooltips to an altdoc site's reference links
#'
#' Runs after `altdoc::render_docs()`. Reads the package's own `man/*.Rd`
#' files (see [build_topic_index()]) and rewrites every `docs/**/*.html`
#' file, attaching a hover tooltip to each reference link that names a
#' documented object in this package. Everything else is left untouched.
#'
#' Idempotent: a file that already carries reftip's marker comment is
#' skipped, so it's safe to rerun after a fresh `render_docs()`.
#'
#' @param path Path to the package root.
#' @param docs_dir Path to the built site. Defaults to `file.path(path,
#'   "docs")`.
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

    index <- build_topic_index(path, quiet = quiet)
    if (length(index) == 0) {
        if (!quiet) message("reftip: no documented topics found in man/*.Rd; nothing to do.")
        return(invisible(list(files = 0L, links = 0L)))
    }

    html_files <- fs::dir_ls(docs_dir, regexp = "\\.html$", recurse = TRUE)
    n_files <- 0L
    n_links <- 0L

    for (f in html_files) {
        html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        if (grepl(.REFTIP_MARKER, html, fixed = TRUE)) next

        man_href_prefix <- .reftip_man_href_prefix(f, docs_dir)
        result <- .inject_tooltips_html(html, index, man_href_prefix)
        if (result$n == 0) next

        new_html <- result$html
        if (grepl("</head>", new_html, fixed = TRUE)) {
            new_html <- sub("</head>", paste0(.reftip_css_block(), "</head>"), new_html, fixed = TRUE)
        }
        body_addition <- paste0(.reftip_payload_html(result$tips), .reftip_script_block())
        # A plain `sub("</body>", ...)` would match the first literal
        # occurrence anywhere in the raw HTML text, including one that
        # happens to sit inside a <script> block's own source (e.g. a JS
        # comment mentioning "</body>"), splicing our payload into the
        # middle of unrelated script text. The real closing tag is always
        # the last one, immediately before the document's closing </html>.
        if (grepl("</body>\\s*</html>\\s*$", new_html, perl = TRUE)) {
            new_html <- sub(
                "</body>(\\s*</html>\\s*)$",
                paste0(body_addition, "</body>\\1"),
                new_html,
                perl = TRUE
            )
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

# Relative path from a rendered page to docs/man/, e.g. "man/" from
# docs/index.html or "../man/" one directory down.
.reftip_man_href_prefix <- function(file, docs_dir) {
    man_dir <- fs::path(docs_dir, "man")
    rel <- fs::path_rel(man_dir, start = fs::path_dir(file))
    paste0(as.character(rel), "/")
}

# One pass over an HTML document: every `.FU_LINK_RE` match that resolves in
# `index` is relinked to docs/man/<topic>.html and tagged with
# `data-reftip="<id>"`; everything else passes through unchanged. `tips`
# collects one entry per unique resolved name (id -> {usage, brief, topic}),
# the page's hidden tooltip payload.
.inject_tooltips_html <- function(html, index, man_href_prefix = "man/") {
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
    tip_ids <- character(0)
    tips <- list()

    for (i in seq_along(starts)) {
        s <- starts[i]
        l <- lens[i]
        is_block <- cap_starts[i, 1] != 0L   # span (fu/va) vs. inline <code>
        span_class <- if (is_block) substr(html, cap_starts[i, 1], cap_starts[i, 1] + cap_lens[i, 1] - 1L) else NA
        name_col <- if (is_block) 2L else 3L
        name_html <- substr(html, cap_starts[i, name_col], cap_starts[i, name_col] + cap_lens[i, name_col] - 1L)
        name <- .html_unescape(name_html)
        # strip a trailing call, e.g. "foo(1, 2)" -> "foo", and a wrapping
        # backtick pair from a literal `` `name<-`() `` in prose
        lookup <- sub("\\(.*$", "", name, perl = TRUE)
        lookup <- sub("^`(.*)`$", "\\1", lookup)

        entry <- index[[lookup]]

        pieces[[pi]] <- substr(html, pos, s - 1L)
        pi <- pi + 1L
        if (is.null(entry)) {
            pieces[[pi]] <- substr(html, s, s + l - 1L)
        } else {
            id <- tip_ids[lookup]
            if (is.na(id)) {
                id <- paste0("t", length(tips) + 1L)
                tip_ids[lookup] <- id
                entry$name <- lookup
                tips[[id]] <- entry
            }
            href <- paste0(man_href_prefix, entry$topic, ".html")
            pieces[[pi]] <- if (is_block) {
                sprintf(
                    '<span class="%s"><a class="reftip-ref" data-reftip="%s" href="%s">%s</a></span>',
                    span_class, id, href, name_html
                )
            } else {
                sprintf(
                    '<code><a class="reftip-ref" data-reftip="%s" href="%s">%s</a></code>',
                    id, href, name_html
                )
            }
            n_linked <- n_linked + 1L
        }
        pi <- pi + 1L
        pos <- s + l
    }
    pieces[[pi]] <- substr(html, pos, nchar(html))

    list(html = paste(unlist(pieces), collapse = ""), n = n_linked, tips = tips)
}
