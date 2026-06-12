# Adds non standard and hidden files to the .`Rbuildignore` file

Adds non standard and hidden files to the .`Rbuildignore` file

## Usage

``` r
fix_non_standard_files(pkg = ".", check)
```

## Arguments

- pkg:

  the package location

- check:

  the results of a
  [`devtools::check`](https://devtools.r-lib.org/reference/check.html)
  command (will check automatically if not provided)

## Value

nothing
