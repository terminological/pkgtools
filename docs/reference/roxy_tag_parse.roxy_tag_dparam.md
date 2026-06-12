# Parser for `@dparam` tags

Replaces the default behaviour of a `@param` tag and documents default
parameters automatically.

## Usage

``` r
# S3 method for class 'roxy_tag_dparam'
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
