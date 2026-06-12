# Adds global variables identified at ``` R CMD check`` to a  ```globals.R\` file

Adds global variables identified at
``` R CMD check`` to a  ```globals.R\` file

## Usage

``` r
fix_global_variables(pkg = ".", check)
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
