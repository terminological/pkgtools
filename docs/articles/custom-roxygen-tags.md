# Custom Roxygen Tags: @unit and @dparam

This vignette describes the custom `roxygen2` tags provided by
`pkgtools`, which integrate unit testing and parameter documentation
directly into your function documentation.

## Setup

To enable pkgtools tags in your package, run once:

``` r
pkgtools::use_unit_test()
```

This adds `pkgtools` to the `Roxygen` packages field and the
`Config/Needs/build` field of your `DESCRIPTION`.

## The `@unit` Tag

The `@unit` tag lets you write unit test code directly in a function’s
roxygen block. When you run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
(or
[`pkgtools::document()`](https://terminological.github.io/pkgtools/reference/document.md)),
the test code is extracted and written to the corresponding test file
under `tests/testthat/`.

### Basic Usage

``` r
#' My function
#'
#' @param x a number
#' @return the square of x
#' @unit
#' testthat::expect_equal(my_func(2), 4)
#' testthat::expect_equal(my_func(0), 0)
my_func <- function(x) {
  x^2
}
```

After running
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html),
the test is written to `tests/testthat/test-unit-my-file.R` (where
`my-file` corresponds to the R file containing `my_func`).

### How It Works

1.  The `@unit` tag must appear inside a roxygen block.
2.  The code between `@unit` and the next roxygen tag (or end of block)
    is extracted as test code.
3.  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    processes all `@unit` blocks and writes test files.
4.  Each test is wrapped in a `test_that()` call with a failure handler
    that links back to the source location.

### Without Explicit Expectations

If no `expect_*` call is found in the `@unit` block, a
[`testthat::expect_no_error`](https://testthat.r-lib.org/reference/expect_no_error.html)
wrapper is used instead:

``` r
#' My function
#' @unit
#' my_func(5)  # will be wrapped in expect_no_error()
my_func <- function(x) x^2
```

### Multiple Functions

Each function can have its own `@unit` block. Tests are grouped by
source file, so `my_func` and another function from the same R file
share one test file.

## The `@dparam` Tag

The `@dparam` tag documents the default value of a function parameter.
It works in conjunction with
[`pkgtools::doc_formals()`](https://terminological.github.io/pkgtools/reference/doc_formals.md)
to automatically generate documentation for parameter defaults.

### Usage

``` r
#' My function
#'
#' @param arg a categorical variable: (default `option one`; allowed: `option one`, `option two`, `option three`)
#' @param n a sample size: (default `123` [integer])
#' @return a vector
#'
my_func <- function(
  arg = c("option one", "option two", "option three"),
  n = 123
) {
  arg = match.arg(arg)
  sample(letters, n, replace = TRUE)
}
```

The
[`doc_formals()`](https://terminological.github.io/pkgtools/reference/doc_formals.md)
call is evaluated at documentation time and inserts the default value
information into the Rd file.

### Supported Patterns

- **[`match.arg()`](https://rdrr.io/r/base/match.arg.html) vectors**:
  Detects [`match.arg()`](https://rdrr.io/r/base/match.arg.html) calls
  and documents both the default and allowed values.
- **Atomic defaults**: Shows the default value and its type.
- **Non-evaluable defaults**: Falls back to showing the unevaluated
  call.

## Integration with `@name`

For topics that don’t have an automatic name (e.g., objects, not
functions), use `@name` to set the topic name:

``` r
#' @name my_object
#' @format A data frame
#' @unit
#' testthat::expect_equal(nrow(my_object), 150)
my_object <- datasets::iris
```

## The `merge_code()` Gadget

When tests are generated or templates are applied,
[`merge_code()`](https://terminological.github.io/pkgtools/reference/merge_code.md)
may launch an interactive Shiny gadget to resolve conflicts. This
provides a three-panel diff viewer (original, suggestion, result) for
merging changes.

## Best Practices

1.  **Keep unit tests close to the function** — the `@unit` tag promotes
    colocation of documentation and tests.
2.  **Use `@unit` for small, focused tests** — complex test fixtures are
    still better as standalone `testthat` blocks.
3.  **Use `@dparam` for parameters with meaningful defaults** — simple
    parameters like `x` don’t need documentation.
4.  **Run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    regularly** — `@unit` tests are only written when documentation is
    regenerated.
5.  **Review generated tests** — the auto-generated test wrapper
    includes source location links; verify they point to the right
    function.
