# Support for `@unit` tags

The `@unit` tag can be used in `roxygen2` documentation of a function to
define unit testing code. This code will be written to a testthat file.

## Usage

``` r
# S3 method for class 'roxy_tag_unit'
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

## Unit tests


    # for testing
    warning("should be suppressed")

## Examples

``` r
# An example function definition:
fn_definition <- "
#' This is a title
#'
#' This is the description.
#'
#' @md
#' @unit
#' # unit testing code here:
#' testthat::expect_equal(f(), \"test\")
#' @export
f <- function() {
  return(\"test\")
}
"

# For this example we manually parse the function specification in `fn_definition`
# creating a .Rd block - normally this is done by `roxygen2` which then
# writes this to an .Rd file. This function is not intended to be used
# outside of a call to `devtools::document`.

fn_definition = gsub("@","@",fn_definition)
tmp = roxygen2::parse_text(fn_definition)
print(tmp[[1]])
#> <roxy_block> [<text>:11]
#>   $tag
#>     [line:  2] @title 'This is a title' {parsed}
#>     [line:  4] @description 'This is the description.' {parsed}
#>     [line:  6] @md '' {parsed}
#>     [line:  7] @unit '...' {parsed}
#>     [line: 10] @export '' {parsed}
#>     [line: 11] @usage '<generated>' {parsed}
#>     [line: 11] @.formals '<generated>' {unparsed}
#>     [line: 11] @backref '<generated>' {parsed}
#>   $call   f <- function() { ...
#>   $object <function> 
#>     $topic f
#>     $alias f
```
