# Install package locally using `renv` if available.

[`devtools::install_local`](https://devtools.r-lib.org/reference/install-deprecated.html)
does not play well with `renv` in this version of `install_local` we
intercept installation of locally developed packages when we are in a
`renv` managed project and installing a local dependency, it builds a
source project into `renv` cellar and installs it from there. This
allows a copy of a locally developed package to be deployed with the
`renv` managed analysis project without specifically being deployed to
`CRAN` or `r-universe`.

## Usage

``` r
install_local(
  path = ".",
  ...,
  force = TRUE,
  upgrade = "never",
  quiet = TRUE,
  wd = here::here()
)
```

## Arguments

- path:

  the package (defaults to `.`)

- ...:

  Arguments passed on to
  [`remotes::install_local`](https://remotes.r-lib.org/reference/install_local.html)

  `subdir`

  :   subdirectory within url bundle that contains the R package.

  `dependencies`

  :   Which dependencies do you want to check? Can be a character vector
      (selecting from "Depends", "Imports", "LinkingTo", "Suggests", or
      "Enhances"), or a logical vector.

      `TRUE` is shorthand for "Depends", "Imports", "LinkingTo" and
      "Suggests". `NA` is shorthand for "Depends", "Imports" and
      "LinkingTo" and is the default. `FALSE` is shorthand for no
      dependencies (i.e. just check this package, not its dependencies).

      The value "soft" means the same as `TRUE`, "hard" means the same
      as `NA`.

      You can also specify dependencies from one or more additional
      fields, common ones include:

      - Config/Needs/website - for dependencies used in building the
        pkgdown site.

      - Config/Needs/coverage for dependencies used in calculating test
        coverage.

  `build`

  :   If `TRUE` build the package before installing.

  `build_opts`

  :   Options to pass to `R CMD build`, only used when `build` is
      `TRUE`.

  `build_manual`

  :   If `FALSE`, don't build PDF manual ('–no-manual').

  `build_vignettes`

  :   If `FALSE`, don't build package vignettes ('–no-build-vignettes').

  `repos`

  :   A character vector giving repositories to use.

  `type`

  :   Type of package to `update`.

- force:

  do the install regardless of versions

- upgrade:

  upgrade out of data CRAN packages

- quiet:

  do it quietly

- wd:

  the project root directory of the current project (defaults to
  [`here::here()`](https://here.r-lib.org/reference/here.html))

## Details

If installed locally for a non-`renv` project (e.g. a package
development) the usual behaviour applies to version management.
Installation of new versions of the project will happen when the package
is released and then installed from the release location (e.g. github,
cran, r-universe).

If a locally developed package is deployed to an `renv` project once it
is released onto a valid distribution platform e.g. CRAN, r-universe or
github, we will want to use that version in our `renv`. This we can do
using the `rebuild = TRUE` option of
[`renv::install`](https://rstudio.github.io/renv/reference/install.html),
e.g.:
`renv::install(...pkgname/github..., repo = ...r-universe?..., rebuild = TRUE)`
followed by a
[`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html)
to update the lock file. The locally built package version will remain
in the `<projroot>/renv/local` cellar until removed by hand.

## See also

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/reference/install_bioc.html),
[`install_bitbucket()`](https://remotes.r-lib.org/reference/install_bitbucket.html),
[`install_cran()`](https://remotes.r-lib.org/reference/install_cran.html),
[`install_dev()`](https://remotes.r-lib.org/reference/install_dev.html),
[`install_github()`](https://remotes.r-lib.org/reference/install_github.html),
[`install_gitlab()`](https://remotes.r-lib.org/reference/install_gitlab.html),
[`install_git()`](https://remotes.r-lib.org/reference/install_git.html),
[`install_svn()`](https://remotes.r-lib.org/reference/install_svn.html),
[`install_url()`](https://remotes.r-lib.org/reference/install_url.html),
[`install_version()`](https://remotes.r-lib.org/reference/install_version.html)

## Examples

``` r
if (FALSE) {
  # Install a local package, using renv if available:
  install_local("/path/to/my/package")
}
```
