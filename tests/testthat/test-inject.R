.sample_html <- function(body) {
    paste0(
        "<html><head><title>t</title></head><body>", body, "</body></html>"
    )
}

test_that("a resolved reference link is tagged and gets a tooltip payload entry", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b."))
    html <- .sample_html(paste0(
        "<code class=\"sourceCode R\"><span class=\"fu\">",
        "<a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>",
        "<span class=\"op\">(</span></code>"
    ))

    result <- .inject_tooltips_html(html, index)

    expect_equal(result$n, 1L)
    expect_length(result$tips, 1L)
    expect_match(result$html, "class=\"reftip-ref\" data-reftip=\"t1\"", fixed = TRUE)
    expect_equal(result$tips[["t1"]]$usage, "foo(a, b)")
})

test_that("a link whose name isn't in the index is left untouched", {
    index <- list(foo = list(usage = "foo(a, b)", brief = NULL))
    original <- paste0(
        "<span class=\"fu\">",
        "<a href=\"https://rdrr.io/r/base/library.html\">library</a></span>"
    )
    html <- .sample_html(original)

    result <- .inject_tooltips_html(html, index)

    expect_equal(result$n, 0L)
    expect_equal(result$html, html)
})

test_that("repeated references to the same name share one tip id", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b."))
    link <- "<span class=\"fu\"><a href=\"foo.html\">foo</a></span>"
    html <- .sample_html(paste(link, link))

    result <- .inject_tooltips_html(html, index)

    expect_equal(result$n, 2L)
    expect_length(result$tips, 1L)
    expect_equal(
        length(gregexpr("data-reftip=\"t1\"", result$html, fixed = TRUE)[[1]]),
        2L
    )
})

test_that("add_tooltips rewrites files on disk and is idempotent on rerun", {
    dir <- withr::local_tempdir()
    fs::dir_create(fs::path_join(c(dir, "man")))
    writeLines(c(
        "\\name{foo}", "\\alias{foo}", "\\title{Foo}",
        "\\usage{", "foo(a, b)", "}",
        "\\description{", "Adds a and b.", "}"
    ), fs::path_join(c(dir, "man", "foo.Rd")))

    docs_dir <- fs::path_join(c(dir, "docs"))
    fs::dir_create(docs_dir)
    page <- fs::path_join(c(docs_dir, "index.html"))
    writeLines(.sample_html(
        "<span class=\"fu\"><a href=\"foo.html\">foo</a></span>"
    ), page)

    res1 <- add_tooltips(path = dir, quiet = TRUE)
    expect_equal(res1$files, 1L)
    expect_equal(res1$links, 1L)

    res2 <- add_tooltips(path = dir, quiet = TRUE)
    expect_equal(res2$files, 0L)
    expect_equal(res2$links, 0L)
})

test_that("add_tooltips errors when docs/ doesn't exist", {
    dir <- withr::local_tempdir()
    expect_error(add_tooltips(path = dir), "render_docs")
})
