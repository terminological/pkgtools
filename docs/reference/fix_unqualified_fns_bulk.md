# Fix unqualified namespaced functions.

This is a code linting function and expected to be called at the console
during package development. It will scan the files in the current
project and replace unqualified references to e.g. `mutate` with ones to
[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)
etc. All changes are made after a commit which can be reverted.

## Usage

``` r
fix_unqualified_fns_bulk(
  pkg = ".",
  rDirectories = c(here::here("R"), here::here("tests/testthat")),
  prioritise = c("dplyr", "rlang", "stringr", "forcats", "ggplot2", "purrr", "tidyr",
    "readr", "stats", "utils")
)
```

## Arguments

- pkg:

  the package

- rDirectories:

  the locations of the R code to fix (by default R scripts, tests, but
  not data-raw or vignettes)

- prioritise:

  a list of package names to pick from first

## Value

a tibble with columns `path`, `pkg`, `name`, and `value` summarising
which unqualified function calls were found in which files, or `NULL` if
no fixes were needed.

## Examples

``` r
if (FALSE) {
  # Fix all unqualified function calls in a package:
  fix_unqualified_fns_bulk()

  # Restrict to specific directories:
  fix_unqualified_fns_bulk(rDirectories = here::here("R"))
}
```
