.write_rd <- function(dir, name, content) {
    fs::dir_create(fs::path_join(c(dir, "man")))
    writeLines(content, fs::path_join(c(dir, "man", paste0(name, ".Rd"))))
}

test_that("build_topic_index extracts usage and first-sentence brief", {
    dir <- withr::local_tempdir()
    .write_rd(dir, "foo", c(
        "\\name{foo}",
        "\\alias{foo}",
        "\\title{Add two numbers}",
        "\\usage{",
        "foo(a, b)",
        "}",
        "\\description{",
        "Adds \\code{a} and \\code{b} together and returns the sum. See details.",
        "}"
    ))

    index <- build_topic_index(dir)

    expect_named(index, "foo")
    expect_equal(index$foo$usage, "foo(a, b)")
    expect_equal(
        index$foo$brief,
        "Adds a and b together and returns the sum."
    )
    expect_equal(index$foo$topic, "foo")
})

test_that("build_topic_index maps every alias of a page to the same brief, and narrows a multi-alias \\usage{} block by name", {
    dir <- withr::local_tempdir()
    .write_rd(dir, "print.foo", c(
        "\\name{print.foo}",
        "\\alias{print.foo}",
        "\\alias{foo}",
        "\\title{Foo methods}",
        "\\usage{",
        "foo(x)",
        "",
        "\\method{print}{foo}(x, ...)",
        "}",
        "\\description{",
        "Prints a foo object.",
        "}"
    ))

    index <- build_topic_index(dir)

    expect_setequal(names(index), c("print.foo", "foo"))
    expect_equal(index[["print.foo"]]$brief, index[["foo"]]$brief)
    expect_equal(index[["foo"]]$usage, "foo(x)")
    expect_equal(index[["print.foo"]]$usage, "print.foo(x, ...)")
    expect_equal(index[["foo"]]$topic, "print.foo")
    expect_equal(index[["print.foo"]]$topic, "print.foo")
})

test_that("an alias with no matching \\usage{} block falls back to the whole block and warns", {
    dir <- withr::local_tempdir()
    .write_rd(dir, "print.foo", c(
        "\\name{print.foo}",
        "\\alias{print.foo}",
        "\\alias{foo}",
        "\\title{Foo methods}",
        "\\usage{",
        "\\method{print}{foo}(x, ...)",
        "}",
        "\\description{",
        "Prints a foo object.",
        "}"
    ))

    expect_warning(index <- build_topic_index(dir), "foo")
    expect_equal(index[["foo"]]$usage, "print.foo(x, ...)")
    expect_no_warning(build_topic_index(dir, quiet = TRUE))
})

test_that("build_topic_index errors without a man/ directory", {
    dir <- withr::local_tempdir()
    expect_error(build_topic_index(dir), "man/")
})

test_that("a description with no sentence boundary within 200 characters warns and clips with '...'", {
    dir <- withr::local_tempdir()
    long_text <- paste(rep("word", 60), collapse = " ")
    .write_rd(dir, "foo", c(
        "\\name{foo}",
        "\\alias{foo}",
        "\\title{Foo}",
        "\\usage{",
        "foo()",
        "}",
        "\\description{",
        long_text,
        "}"
    ))

    expect_warning(index <- build_topic_index(dir), "truncated mid-sentence")
    expect_true(endsWith(index$foo$brief, "..."))
    expect_no_warning(build_topic_index(dir, quiet = TRUE))
})

test_that("a topic with no description prose has a NULL brief", {
    dir <- withr::local_tempdir()
    .write_rd(dir, "bar", c(
        "\\name{bar}",
        "\\alias{bar}",
        "\\title{Bar}",
        "\\usage{",
        "bar()",
        "}",
        "\\description{",
        "}"
    ))

    index <- build_topic_index(dir)
    expect_null(index$bar$brief)
})
