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
})

test_that("build_topic_index maps every alias of a page to the same entry", {
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

    index <- build_topic_index(dir)

    expect_setequal(names(index), c("print.foo", "foo"))
    expect_equal(index[["print.foo"]]$brief, index[["foo"]]$brief)
})

test_that("build_topic_index errors without a man/ directory", {
    dir <- withr::local_tempdir()
    expect_error(build_topic_index(dir), "man/")
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
