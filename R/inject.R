# Rewrites Quarto's rendered HTML so references to this package's own
# documented topics get a hover tooltip, instead of relying on downlit alone
# (which can't resolve a name it can't statically dispatch, e.g. an S3
# method called by name, and, on an altdoc site, can't resolve a local/
# unpublished package at all).
#
# Two reference shapes are matched, each optionally already wrapped in a
# downlit link:
#  1. A called function inside a fenced code block: Pandoc tags it
#     `<span class="fu">name</span>` (or "va" for a referenced-but-not-
#     called value, e.g. an R6 class name).
#  2. Inline code in prose (`` `foo()` ``): `<code>name()</code>`.
# Both may already carry an `<a href="...">` from downlit. On altdoc that
# href is wrong for our own package (downlit built against a name it
# couldn't resolve locally) and is always replaced. On pkgdown, downlit
# builds against the installed local package and already gets it right, so
# the existing href is kept and only a tooltip is attached; the href is
# built from scratch only when downlit left the name unlinked.

.FU_LINK_RE <- paste0(
    "<span class=\"(fu|va)\">(?:<a href=\"([^\"]*)\">)?([^<]*?)(?:</a>)?</span>",
    "|",
    "<code>(?:<a href=\"([^\"]*)\">)?([^<]*?)(?:</a>)?</code>"
)

#' Add hover tooltips to an altdoc or pkgdown site's reference links
#'
#' Run after you build docs with `altdoc::render_docs()` or
#' `pkgdown::build_site()`. Reads the package's own `man/*.Rd` files (see
#' [build_topic_index()]) and rewrites every `docs/**/*.html` file,
#' attaching a hover tooltip to each reference link that names a documented
#' object in this package. Everything else is left untouched.
#'
#' On an altdoc site, downlit can't resolve the package's own (local,
#' unpublished) functions, so reftip also relinks each reference straight to
#' `docs/man/<topic>.html`. On a pkgdown site, downlit already resolves
#' these correctly (it builds against the installed local package), so
#' reftip leaves the existing link alone and only attaches the tooltip;
#' it builds a link itself only for a name downlit left unlinked (e.g. an S3
#' method called by name).
#'
#' Idempotent: a file that already carries reftip's marker comment is
#' skipped, so it's safe to rerun after a fresh build.
#'
#' @param path Path to the package root.
#' @param docs_dir Path to the built site. Defaults to `file.path(path,
#'   "docs")`.
#' @param site One of `"auto"` (default), `"altdoc"`, or `"pkgdown"`.
#'   `"auto"` detects the site type from `docs_dir`'s layout: a `reference/`
#'   subdirectory means pkgdown, a `man/` subdirectory means altdoc.
#' @param quiet Logical. Suppress progress messages.
#' @return Invisibly, a list with `files` (how many HTML files were touched)
#'   and `links` (how many reference links got a tooltip).
#' @export
add_tooltips <- function(path = ".", docs_dir = NULL, site = c("auto", "altdoc", "pkgdown"), quiet = FALSE) {
    site <- match.arg(site)
    if (is.null(docs_dir)) {
        docs_dir <- fs::path_join(c(path, "docs"))
    }
    if (!fs::dir_exists(docs_dir)) {
        stop(
            sprintf(
                "No built site found at '%s'. Run altdoc::render_docs() or pkgdown::build_site() first.",
                docs_dir
            ),
            call. = FALSE
        )
    }
    site <- .reftip_resolve_site(site, docs_dir)
    reference_dir <- if (site == "pkgdown") "reference" else "man"

    index <- build_topic_index(path, quiet = quiet)
    if (length(index) == 0) {
        if (!quiet) {
            message(
                "reftip: no documented topics found in man/*.Rd; nothing to do."
            )
        }
        return(invisible(list(files = 0L, links = 0L)))
    }

    html_files <- fs::dir_ls(docs_dir, regexp = "\\.html$", recurse = TRUE)
    n_files <- 0L
    n_links <- 0L

    for (f in html_files) {
        html <- paste(
            readLines(f, warn = FALSE, encoding = "UTF-8"),
            collapse = "\n"
        )
        if (grepl(.REFTIP_MARKER, html, fixed = TRUE)) {
            next
        }

        href_prefix <- .reftip_reference_href_prefix(f, docs_dir, reference_dir)
        result <- .inject_tooltips_html(html, index, href_prefix, overwrite_href = site != "pkgdown")
        if (result$n == 0) {
            next
        }

        new_html <- result$html
        if (grepl("</head>", new_html, fixed = TRUE)) {
            new_html <- sub(
                "</head>",
                paste0(.reftip_css_block(), "</head>"),
                new_html,
                fixed = TRUE
            )
        }
        body_addition <- paste0(
            .reftip_payload_html(result$tips),
            .reftip_script_block()
        )
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
            n_links,
            if (n_links == 1) "" else "s",
            n_files,
            if (n_files == 1) "" else "s"
        ))
    }

    invisible(list(files = n_files, links = n_links))
}

# Detects the site type from docs_dir's layout when site == "auto"; errors
# if neither a reference/ (pkgdown) nor a man/ (altdoc) subdirectory is
# found, or a specific site was requested but its subdirectory is missing.
.reftip_resolve_site <- function(site, docs_dir) {
    has_reference <- fs::dir_exists(fs::path(docs_dir, "reference"))
    has_man <- fs::dir_exists(fs::path(docs_dir, "man"))

    if (site == "auto") {
        if (has_reference && !has_man) {
            return("pkgdown")
        }
        if (has_man && !has_reference) {
            return("altdoc")
        }
        stop(
            sprintf(
                "reftip: couldn't detect the site type from '%s' (found %s). Pass site = \"altdoc\" or site = \"pkgdown\" explicitly.",
                docs_dir,
                if (has_reference && has_man) "both reference/ and man/" else "neither reference/ nor man/"
            ),
            call. = FALSE
        )
    }

    needed <- if (site == "pkgdown") "reference" else "man"
    if (!fs::dir_exists(fs::path(docs_dir, needed))) {
        stop(
            sprintf("reftip: site = \"%s\" but '%s' has no '%s/' subdirectory.", site, docs_dir, needed),
            call. = FALSE
        )
    }
    site
}

# Relative path from a rendered page to docs/<reference_dir>/, e.g. "man/"
# from docs/index.html or "../reference/" one directory down.
.reftip_reference_href_prefix <- function(file, docs_dir, reference_dir = "man") {
    ref_dir <- fs::path(docs_dir, reference_dir)
    rel <- fs::path_rel(ref_dir, start = fs::path_dir(file))
    paste0(as.character(rel), "/")
}

# One pass over an HTML document: every `.FU_LINK_RE` match that resolves in
# `index` is tagged with `data-reftip="<id>"`; everything else passes
# through unchanged. When `overwrite_href` is TRUE (altdoc), the href is
# always rebuilt from `href_prefix`; when FALSE (pkgdown), an existing
# downlit href is kept as-is and `href_prefix` is used only as a fallback
# for a name downlit left unlinked. `tips` collects one entry per unique
# resolved name (id -> {usage, brief, topic}), the page's hidden tooltip
# payload.
.inject_tooltips_html <- function(html, index, href_prefix = "man/", overwrite_href = TRUE) {
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
        is_block <- cap_starts[i, 1] != 0L # span (fu/va) vs. inline <code>
        span_class <- if (is_block) {
            substr(
                html,
                cap_starts[i, 1],
                cap_starts[i, 1] + cap_lens[i, 1] - 1L
            )
        } else {
            NA
        }
        href_col <- if (is_block) 2L else 4L
        name_col <- if (is_block) 3L else 5L
        name_html <- substr(
            html,
            cap_starts[i, name_col],
            cap_starts[i, name_col] + cap_lens[i, name_col] - 1L
        )
        existing_href <- if (cap_starts[i, href_col] > 0L) {
            substr(
                html,
                cap_starts[i, href_col],
                cap_starts[i, href_col] + cap_lens[i, href_col] - 1L
            )
        } else {
            NA_character_
        }
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
            href <- if (!overwrite_href && !is.na(existing_href) && nzchar(existing_href)) {
                existing_href
            } else {
                paste0(href_prefix, entry$topic, ".html")
            }
            pieces[[pi]] <- if (is_block) {
                sprintf(
                    '<span class="%s"><a class="reftip-ref" data-reftip="%s" href="%s">%s</a></span>',
                    span_class,
                    id,
                    href,
                    name_html
                )
            } else {
                sprintf(
                    '<code><a class="reftip-ref" data-reftip="%s" href="%s">%s</a></code>',
                    id,
                    href,
                    name_html
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
