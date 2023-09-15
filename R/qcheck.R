
#' Check the package structure without running any code
#'
#' @inheritDotParams devtools::check
#' @param quiet do it without producing messages
#'
#' @return a check result
#' @export
qcheck = function(..., quiet = FALSE) {
  if (quiet) {
    message("running R CMD check...",appendLF = FALSE)
    check = suppressMessages(devtools::check(args = c("--no-examples", "--no-tests", "--ignore-vignettes"), quiet=quiet))
    message("COMPLETE")
  } else {
    check = devtools::check(args = c("--no-examples", "--no-tests", "--ignore-vignettes"), ...)
  }
  invisible(check)
}