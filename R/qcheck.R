#' Check the package structure without running any code
#'
#' Runs `R CMD check` with `--no-examples --no-tests --ignore-vignettes` to
#' catch structural issues (imports, global variables, encoding) without
#' executing code. Also optionally runs `spelling::spell_check_package()` and
#' `urlchecker::url_check()` if those packages are installed.
#'
#' @inheritDotParams devtools::check
#' @param pkg the path of the package to check
#' @param args additional r cmd check args
#' @param quiet do it without producing messages
#'
#' @concept usethis
#'
#' @return a check result (list) or `invisible(check)`
#' @export
#' @examples
#' if (FALSE) {
#'   check <- qcheck()
#'   print(names(check))  # warnings, notes, errors, etc.
#' }
qcheck <- function(pkg = ".", ..., args = "", quiet = FALSE) {
  pkg <- devtools::as.package(pkg)
  if (quiet) {
    message("running R CMD check...", appendLF = FALSE)
    check <- suppressMessages(devtools::check(
      pkg = pkg$path,
      ...,
      args = c("--no-examples", "--no-tests", "--ignore-vignettes", args),
      quiet = quiet
    ))
    message("COMPLETE")
  } else {
    check <- devtools::check(
      pkg = pkg$path,
      ...,
      args = c("--no-examples", "--no-tests", "--ignore-vignettes", args)
    )
  }
  if (requireNamespace("spelling", quietly = TRUE)) {
    spelling::spell_check_package()
  }
  if (requireNamespace("urlchecker", quietly = TRUE)) {
    urlchecker::url_check()
  }
  invisible(check)
}
