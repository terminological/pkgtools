# Set up a package for pkgtools `@unit` tag usage

This function amends `DESCRIPTION` to register pkgtools as a build-time
dependency and adds `pkgtools` to the Roxygen `packages` field. Call it
once when setting up a new package that will use `@unit` and `@dparam`
custom roxygen tags.

## Usage

``` r
use_unit_test(path = ".")
```

## Arguments

- path:

  character path to the package

## Value

Nothing: called for file system side effects.

## Examples

``` r
if (FALSE) {
  use_unit_test("/path/to/my/package")
}
```
