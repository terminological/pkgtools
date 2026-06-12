# Migrate package data to a github hosted pin board

This function makes multiple changes to the package. It adds packaged
data files from the `data` directory to a `pkgdown` github published pin
board and constructs a minimal set of functions with the same names as
the data to access the data from the pinboard. It finds any references
to the dataset in the code directories
(`data-raw`,`R`,`vignettes`,`test`) and replaces them with a function
call of the same name.

## Usage

``` r
migrate_to_pins(pkg = ".", board = "data", migrate = NULL)
```

## Arguments

- pkg:

  the package (defaults to current)

- board:

  the board name (this is generally not exposed to user)

- migrate:

  either "\*" for all data files, a list of names or if null the user
  will be asked interactively.

## Value

nothing

## Examples

``` r
if (FALSE) {
  # Migrate all package data to a pins board:
  migrate_to_pins()

  # Migrate specific datasets:
  migrate_to_pins(migrate = c("mtcars", "iris"))
}
```
