# Adds new repositories to the beginning of an `renv` lockfile

Sets custom repositories (e.g. r-universe repositories) in a `renv`
lockfile to override CRAN repositories. This is a persistent change can
be undone by manual editing of the lockfile.

## Usage

``` r
set_renv_repos(..., .wd = here::here())
```

## Arguments

- ...:

  a named list of repository urls

- .wd:

  the working directory (defaults to
  [`here::here()`](https://here.r-lib.org/reference/here.html))

## Value

nothing
