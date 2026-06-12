# Check the package structure without running any code

Runs `R CMD check` with `--no-examples --no-tests --ignore-vignettes` to
catch structural issues (imports, global variables, encoding) without
executing code. Also optionally runs `spelling::spell_check_package()`
and
[`urlchecker::url_check()`](https://rdrr.io/pkg/urlchecker/man/url_check.html)
if those packages are installed.

## Usage

``` r
qcheck(pkg = ".", ..., args = "", quiet = FALSE)
```

## Arguments

- pkg:

  the path of the package to check

- ...:

  Arguments passed on to
  [`devtools::check`](https://devtools.r-lib.org/reference/check.html)

  `document`

  :   By default (`NULL`) will document if your installed roxygen2
      version matches the version declared in the `DESCRIPTION` file.
      Use `TRUE` or `FALSE` to override the default.

  `build_args`

  :   Additional arguments passed to `R CMD build`

  `manual`

  :   If `FALSE`, don't build and check manual (`--no-manual`).

  `cran`

  :   if `TRUE` (the default), check using the same settings as CRAN
      uses. Because this is a moving target and is not uniform across
      all of CRAN's machine, this is on a "best effort" basis. It is
      more complicated than simply setting `--as-cran`.

  `remote`

  :   Sets `_R_CHECK_CRAN_INCOMING_REMOTE_` env var. If `TRUE`, performs
      a number of CRAN incoming checks that require remote access.

  `incoming`

  :   Sets `_R_CHECK_CRAN_INCOMING_` env var. If `TRUE`, performs a
      number of CRAN incoming checks.

  `force_suggests`

  :   Sets `_R_CHECK_FORCE_SUGGESTS_`. If `FALSE` (the default), check
      will proceed even if all suggested packages aren't found.

  `run_dont_test`

  :   Sets `--run-donttest` so that examples surrounded in `\donttest{}`
      are also run. When `cran = TRUE`, this only affects R 3.6 and
      earlier; in R 4.0, code in `\donttest{}` is always run as part of
      CRAN submission.

  `env_vars`

  :   Environment variables set during `R CMD check`

  `check_dir`

  :   Path to a directory where the check is performed. If this is not
      `NULL`, then the a temporary directory is used, that is cleaned up
      when the returned object is garbage collected.

  `cleanup`

  :   **\[deprecated\]** See `check_dir` for details.

  `vignettes`

  :   If `FALSE`, do not build or check vignettes, equivalent to using
      `args = '--ignore-vignettes'` and
      `build_args = '--no-build-vignettes'`.

  `error_on`

  :   Whether to throw an error on `R CMD check` failures. Note that the
      check is always completed (unless a timeout happens), and the
      error is only thrown after completion.

      `error_on` is passed through to
      [`rcmdcheck::rcmdcheck()`](http://r-lib.github.io/rcmdcheck/reference/rcmdcheck.md),
      which is the definitive source for what the different values mean.
      If not specified by the user, both `check()` and `check_built()`
      default to `error_on = "never"` in interactive use and `"warning"`
      in a non-interactive setting.

  `path`

  :   Path to built package.

- args:

  additional r cmd check args

- quiet:

  do it without producing messages

## Value

a check result (list) or `invisible(check)`

## Examples

``` r
if (FALSE) {
  check <- qcheck()
  print(names(check))  # warnings, notes, errors, etc.
}
```
