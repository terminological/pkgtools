# Document default formal parameters in a function

this is intended to be used in a Roxygen tag to document the default
value or allowable values of a function

## Usage

``` r
doc_formals(fn, param = NULL, dname = deparse(substitute(param)))
```

## Arguments

- fn:

  a function in the current package

- param:

  a parameter (usually the same as the param block)

## Value

a formatted string for inclusion in a Roxygen block

## Examples

``` r
test_fn = function(
    arg = c("option one", "option two", "option three"),
    def = 123,
    ghi = def * 2
) {
  arg = match.arg(arg)
}

# In a roxygen block:
# \deqn{r}{doc_formals(test_fn, arg)}

doc_formals(test_fn, arg)
#> [1] "(default \\code{`option one  `}; allowed: \\code{`option one  `,`option two  `,`option three`})"
doc_formals(test_fn, def)
#> [1] "(default \\code{`123`} [numeric])"
doc_formals(test_fn, ghi)
#> [1] "(default \\code{`def * 2`})"
#' A test function
#'
#' @param arg a set of descriptions: `r doc_formals(test_fn, arg)`
#' @param def a set of descriptions: `r doc_formals(test_fn, def)`
#' @param ghi a set of descriptions: `r doc_formals(test_fn, ghi)`
#'
#' @ return nothing
test_fn = function(
    arg = c("option one","option two","option three"),
    def = 123,
    ghi = def*2
) {
  arg = match.arg(arg)
}

# @param arg a set of descriptions: `r doc_formals(test_fn, arg)`

doc_formals(test_fn, arg)
#> [1] "(default \\code{`option one  `}; allowed: \\code{`option one  `,`option two  `,`option three`})"
doc_formals(test_fn, def)
#> [1] "(default \\code{`123`} [numeric])"
doc_formals(test_fn, geh)
#> [1] "(default \\code{`NULL`} [NULL])"
```
