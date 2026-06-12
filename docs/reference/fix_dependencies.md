# Fixes dependencies in the namespace file using the output of R CMD check.

Fixes dependencies in the namespace file using the output of R CMD
check.

## Usage

``` r
fix_dependencies(pkg = ".", check)
```

## Arguments

- pkg:

  the package to scan

- check:

  output of a
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  command (will check automatically if not provided)

## Value

nothing
