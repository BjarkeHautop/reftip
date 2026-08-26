.sample_html <- function(body) {
    paste0(
        "<html><head><title>t</title></head><body>", body, "</body></html>"
    )
}

test_that("a downlit-resolved reference link is relinked to the local man page and gets a tooltip", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo"))
    html <- .sample_html(paste0(
        "<code class=\"sourceCode R\"><span class=\"fu\">",
        "<a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>",
        "<span class=\"op\">(</span></code>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_length(result$tips, 1L)
    expect_match(result$html, "class=\"reftip-ref\" data-reftip=\"t1\" href=\"man/foo.html\"", fixed = TRUE)
    expect_equal(result$tips[["t1"]]$usage, "foo(a, b)")
})

test_that("an unresolved (bare) fu span is linked and tagged too", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo"))
    html <- .sample_html(paste0(
        "<code class=\"sourceCode R\"><span class=\"fu\">foo</span>",
        "<span class=\"op\">(</span></code>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_match(result$html, '<span class="fu"><a class="reftip-ref" data-reftip="t1" href="man/foo.html">foo</a></span>', fixed = TRUE)
})

test_that("a va-class span (e.g. an R6 class name used as a value, not a call) is tagged too", {
    index <- list(Counter = list(usage = NULL, brief = "An R6 counter.", topic = "Counter"))
    html <- .sample_html(paste0(
        "<span class=\"va\">",
        "<a href=\"https://rdrr.io/pkg/p/man/Counter.html\">Counter</a></span>",
        "<span class=\"op\">$</span><span class=\"fu\">new</span>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_match(result$html, '<span class="va"><a class="reftip-ref"', fixed = TRUE)
    expect_match(result$html, 'href="man/Counter.html"', fixed = TRUE)
    expect_equal(result$tips[["t1"]]$name, "Counter")
})

test_that("a topic with no \\usage{} (e.g. an R6 class) shows its name, not the tip id, as the fallback signature", {
    tips <- list(t1 = list(usage = NULL, brief = "An R6 counter.", name = "Counter"))
    payload <- .reftip_payload_html(tips)
    expect_match(payload, '<code class="reftip-sig">Counter</code>', fixed = TRUE)
})

test_that("an inline code reference (no fu span), already linked by downlit, is relinked and tagged", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo"))
    html <- .sample_html(paste0(
        "<p>Add numbers with <code>",
        "<a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo()</a></code>:</p>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_length(result$tips, 1L)
    expect_match(result$html, "class=\"reftip-ref\" data-reftip=\"t1\" href=\"man/foo.html\"", fixed = TRUE)
    expect_match(result$html, "<code><a class=\"reftip-ref\"", fixed = TRUE)
    expect_equal(result$tips[["t1"]]$usage, "foo(a, b)")
})

test_that("an inline code reference with no downlit link at all (e.g. an S3 method written out by name) is linked and tagged", {
    index <- list(print.animal = list(usage = "print.animal(x, ...)", brief = "Prints an animal.", topic = "animal"))
    html <- .sample_html("<p>See <code>print.animal()</code> for details.</p>")

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_match(result$html, "<code><a class=\"reftip-ref\" data-reftip=\"t1\" href=\"man/animal.html\">print.animal()</a></code>", fixed = TRUE)
})

test_that("an inline code reference to a backtick-quoted name (e.g. a replacement function) resolves through its literal backticks", {
    index <- list(`name<-` = list(usage = "name(x) <- value", brief = "Set the name.", topic = "animal"))
    html <- .sample_html(paste0(
        "<p>See <code>",
        "<a href=\"https://rdrr.io/pkg/p/man/animal.html\">`name&lt;-`()</a></code>.</p>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
    expect_equal(result$tips[["t1"]]$usage, "name(x) <- value")
})

test_that("an inline code block (with a fu span) doesn't also match the inline pattern", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo"))
    html <- .sample_html(paste0(
        "<code class=\"sourceCode R\"><span class=\"fu\">",
        "<a href=\"foo.html\">foo</a></span>",
        "<span class=\"op\">(</span></code>"
    ))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 1L)
})

test_that("a link whose name isn't in the index is left completely untouched, downlit href and all", {
    index <- list(foo = list(usage = "foo(a, b)", brief = NULL, topic = "foo"))
    original <- paste0(
        "<span class=\"fu\">",
        "<a href=\"https://rdrr.io/r/base/library.html\">library</a></span>"
    )
    html <- .sample_html(original)

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 0L)
    expect_equal(result$html, html)
})

test_that("a bare (unresolved) span whose name isn't in the index is left untouched", {
    index <- list(foo = list(usage = "foo(a, b)", brief = NULL, topic = "foo"))
    html <- .sample_html("<span class=\"fu\">bar</span>")

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 0L)
    expect_equal(result$html, html)
})

test_that("repeated references to the same name share one tip id", {
    index <- list(foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo"))
    link <- "<span class=\"fu\"><a href=\"foo.html\">foo</a></span>"
    html <- .sample_html(paste(link, link))

    result <- .inject_tooltips_html(html, index, "man/")

    expect_equal(result$n, 2L)
    expect_length(result$tips, 1L)
    expect_equal(
        length(gregexpr("data-reftip=\"t1\"", result$html, fixed = TRUE)[[1]]),
        2L
    )
})

test_that(".reftip_man_href_prefix points at docs/man/ relative to the file's own depth", {
    expect_equal(.reftip_man_href_prefix("/x/docs/index.html", "/x/docs"), "man/")
    expect_equal(.reftip_man_href_prefix("/x/docs/vignettes/using.html", "/x/docs"), "../man/")
    expect_equal(.reftip_man_href_prefix("/x/docs/man/foo.html", "/x/docs"), "./")
})

test_that("add_tooltips rewrites files on disk, links locally, and is idempotent on rerun", {
    dir <- withr::local_tempdir()
    fs::dir_create(fs::path_join(c(dir, "man")))
    writeLines(c(
        "\\name{foo}", "\\alias{foo}", "\\title{Foo}",
        "\\usage{", "foo(a, b)", "}",
        "\\description{", "Adds a and b.", "}"
    ), fs::path_join(c(dir, "man", "foo.Rd")))

    docs_dir <- fs::path_join(c(dir, "docs"))
    fs::dir_create(fs::path_join(c(docs_dir, "man")))
    page <- fs::path_join(c(docs_dir, "index.html"))
    writeLines(.sample_html(
        "<span class=\"fu\"><a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>"
    ), page)

    res1 <- add_tooltips(path = dir, quiet = TRUE)
    expect_equal(res1$files, 1L)
    expect_equal(res1$links, 1L)
    expect_match(readLines(page, warn = FALSE), 'href="man/foo.html"', fixed = TRUE, all = FALSE)

    res2 <- add_tooltips(path = dir, quiet = TRUE)
    expect_equal(res2$files, 0L)
    expect_equal(res2$links, 0L)
})

test_that("add_tooltips errors when docs/ doesn't exist", {
    dir <- withr::local_tempdir()
    expect_error(add_tooltips(path = dir), "render_docs")
})
