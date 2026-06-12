# Parser for `@unit` tags

The `@unit` tag can be used in `roxygen2` documentation of a function to
define unit testing code. This code will be written to a testthat file.

## Usage

``` r
# S3 method for class 'roxy_tag_unit'
roxy_tag_parse(x)
```

## Arguments

- x:

  A tag

## Value

a `roxy_tag` object with the `val` field set to the parsed value

## Examples

``` r
# This provides support to `roxygen2` and only gets executed in the context
# of `devtools::document()`. There is no interactive use of this function.
```
