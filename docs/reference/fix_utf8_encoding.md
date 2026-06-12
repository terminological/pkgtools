# Fix non-ASCII (UTF-8) characters in source files

Identifies source files flagged by `R CMD check` as containing non-ASCII
characters, then replaces them with escaped `\uXXXX` sequences. All
changes are committed before making modifications, so they can be
reverted with
[`undo()`](https://terminological.github.io/pkgtools/reference/undo.md).

## Usage

``` r
fix_utf8_encoding(pkg = ".", check)
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
