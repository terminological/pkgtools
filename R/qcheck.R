
#' Check the package structure without running any code
#'
#' @inheritDotParams devtools::check
#' @param pkg the path of the package to check
#' @param args additional r cmd check args
#' @param quiet do it without producing messages
#'
#' @return a check result
#' @export
qcheck = function(pkg = ".", ..., args = "", quiet = FALSE) {
  pkg = devtools::as.package(pkg)
  if (quiet) {
    message("running R CMD check...",appendLF = FALSE)
    check = suppressMessages(devtools::check(pkg = pkg$path, ..., args = c("--no-examples", "--no-tests", "--ignore-vignettes", args), quiet=quiet))
    message("COMPLETE")
  } else {
    check = devtools::check(pkg = pkg$path, ..., args = c("--no-examples", "--no-tests", "--ignore-vignettes", args))
  }
  spelling::spell_check_package()
  urlchecker::url_check()
  invisible(check)
}