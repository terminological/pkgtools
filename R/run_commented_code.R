#' Execute commented out code
#'
#' RStudio does this for examples but nothing else this allows singly commented
#' code or code in other tags e.g. `@unit` to be executed
#'
#' To enable keyboard shortcuts we need the `shrtcts` package and `~/.shrtcts.R`
#' ```
#' #' Execute commented code
#' #'
#' #' Run multi line code stripping off leading comments
#' #'
#' #' @shortcut Ctrl+Shift+P
#' pkgtools::run_commented_code
#' ```
#' and:
#'
#' in `~/.Rprofile`
#' ```
#' if (interactive() && requireNamespace("shrtcts", quietly = TRUE)) {
#'   shrtcts::add_rstudio_shortcuts(set_keyboard_shortcuts = TRUE)
#' }
#' ```
#' @concept edit
#' @returns nothing
#' @export
run_commented_code = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(context$id)
  selection = gsub("(^|\\n)\\s*#'?", "\\1", selection)
  tmp = try(parse(text = selection), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    rstudioapi::showDialog("Invalid selection", format(tmp))
  } else {
    rstudioapi::sendToConsole(selection)
  }
  invisible()
}
