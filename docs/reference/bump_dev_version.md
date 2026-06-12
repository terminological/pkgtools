# Update the version of a package, incrementing dev versions.

Increments the dev component (`.8999` → `.9000`) of a package
`DESCRIPTION` version number. This is used by
[`unstable()`](https://terminological.github.io/pkgtools/reference/unstable.md)
to trigger local reinstall.

## Usage

``` r
bump_dev_version(pkg = ".")
```

## Arguments

- pkg:

  the path to the package

## Value

the new version as a string (e.g. `"1.0.0.9000"`)
