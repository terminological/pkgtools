# Document package and non-package projects

[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
requires a DESCRIPTION file, which in general we don't include in
analysis projects. Sometimes we want to produce development
documentation for functions in analysis projects so we can describe
options and behaviour. `pkgtools::document` will also work in a non
package project to produce `.Rd` files in the `man` directory for any
functions declared in the `R` subfolder. It also override the default
behaviour of `?` to return the development documentation of matching
functions in the current project.

## Usage

``` r
document(project.dir = ".", roclets = NULL, quiet = FALSE)
```

## Arguments

- project.dir:

  the directory of the current (non package project)

- roclets:

  Character vector of roclet names to use with package. The default,
  `NULL`, uses the roxygen `roclets` option, which defaults to
  `c("collate", "namespace", "rd")`.

- quiet:

  if `TRUE` suppresses output from this function.

## Value

nothing - called for side effects

## Examples

``` r
if (FALSE) {
  # Document a package:
  document()

  # Document a non-package project:
  document("/path/to/analysis/project")
}
```
