# Generate a hash based expectation for a standalone object

`testthat` does not use hash based checking anymore, in favour of
snapshots. These don't work well in tests of standalones because the
file will move projects independently of the test directory. This
function lets you quickly create a check based on a gold standard.

## Usage

``` r
standalone_snapshot(obj, .clip = interactive(), .as = deparse(substitute(obj)))
```

## Arguments

- obj:

  the gold standard to test

- .clip:

  copy the result to the clipboard (needs `clipr`)

- .as:

  what name to use for the comparison

## Value

a character string with an `expect_equal()` expression, or
`invisible(out)` if `.clip = TRUE` and `clipr` is available
