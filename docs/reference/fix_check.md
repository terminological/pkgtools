# Fix common check errors

This function runs a set of common package development errors. It will
create a commit before running checks and fixing unqualified functions,
excluding non standard files, creating global variables, fixng utf8
encoding issues and ensuring all dependencies line up.

## Usage

``` r
fix_check(pkg = ".")
```

## Arguments

- pkg:

  the package to fix

## Value

nothing
