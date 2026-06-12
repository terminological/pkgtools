# Support for `@dparam` tags

Replaces the default behaviour of a `@param` tag and documents default
parameters automatically.

## Usage

``` r
# S3 method for class 'roxy_tag_dparam'
roxy_tag_rd(x, base_path, env)
```

## Arguments

- x:

  The tag

- base_path:

  Path to package root directory.

- env:

  Environment in which to evaluate code (if needed)

## Value

an
[`roxygen2::rd_section`](https://roxygen2.r-lib.org/reference/rd_section.html)
(see `roxygen2` documentation)

## Examples

``` r
# An example function definition:
fn_definition <- "
#' This is a title
#'
#' This is the description.
#'
#' @dparam x this parameter specifies a default
#'
f <- function(x = c(\"hello\",\"world\")) {
  return(\"test\")
}
"

# For this example we manually parse the function specification in `fn_definition`
# creating a .Rd block - normally this is done by `roxygen2` which then
# writes this to an .Rd file. This function is not intended to be used
# outside of a call to `devtools::document`.

tmp = roxygen2::parse_text(fn_definition)
print(tmp[[1]])
#> <roxy_block> [<text>:8]
#>   $tag
#>     [line:  2] @title 'This is a title' {parsed}
#>     [line:  4] @description 'This is the description.' {parsed}
#>     [line:  6] @dparam 'x this parameter specifies a default
#> ' {parsed}
#>     [line:  8] @usage '<generated>' {parsed}
#>     [line:  8] @.formals '<generated>' {parsed}
#>     [line:  8] @backref '<generated>' {parsed}
#>   $call   f <- function(x = c("hello", "world")) { ...
#>   $object <function> 
#>     $topic f
#>     $alias f
```
