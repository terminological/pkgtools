# Reload a set of packages that are in development on the local machine

Vignette building uses a new session. Any changes in current project or
dependent locally developed projects are not tested unless the packages
are all installed using `devtools::install_local(...)`. This causes
problems when developing multiple packages in parallel.

## Usage

``` r
unstable(
  path = ".",
  ...,
  force = TRUE,
  upgrade = "never",
  quiet = TRUE,
  load_lib = TRUE
)
```

## Arguments

- path:

  the package local development repository path. This assumes you have
  all your other package code in a sibling directory, e.g. `~/Git/pkg1`,
  `~/Git/pkg2`

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

  Force installation, even if the remote state has not changed since the
  previous install.

- upgrade:

  Should package dependencies be upgraded? One of "default", "ask",
  "always", or "never". "default" respects the value of the
  `R_REMOTES_UPGRADE` environment variable if set, and falls back to
  "ask" if unset. "ask" prompts the user for which out of date packages
  to upgrade. For non-interactive sessions "ask" is equivalent to
  "always". `TRUE` and `FALSE` are also accepted and correspond to
  "always" and "never" respectively.

- quiet:

  If `TRUE`, suppress output.

- load_lib:

  load the package using a library command

## Value

a tibble with columns `package`, `gitpath`, `gitmoddate`, `version` (new
version), and `old_version`, or `NULL` if there were no packages to
update.

## Details

This function assumes the `path` variable is a path to a package which
is under version control in a Git directory. Other dependencies to this
package may also be under development in sibling directories. The aim is
to install the current version of the target package and all locally
held dependencies that have changed on the local disk compared to the
locally installed version.

This function scans the current package and first order dependencies,
looking for local development directories for any packages imported.
Looks for changes in files in local development directories of package
and first order dependencies versus files currently installed in
r-library. If it finds any differences it checks if there is a version
change of the package, bumps the version number of the development
package, and installs it locally, After installation it restarts R.

Any recent file change in development directories triggers a dev version
bump and local package installation. After a call to `unstable()` any
dependencies in your local dev environment are up to date.

If `unstable` is called from within a non package project which is using
`renv` then rather than installing locally using `devtools` the package
is built and deployed locally in the `renv` local package directory (
`<proj root>/renv/local`) and installed from there. The `renv` local
packages are placed under version control. At the moment it is a manual
job to tidy this up once the development package is finalised and
deployed

## Examples

``` r
if (FALSE) {
  # Update the current package and all local dependencies:
  unstable()

  # Update a specific package path:
  unstable("/home/user/Git/myotherpackage")
}
```
