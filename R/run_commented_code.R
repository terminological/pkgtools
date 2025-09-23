#' Execute commented out code
#'
#' RStudio does this for examples but nothing else this allows singly commented
#' code or code in other tags e.g. `@unit` to be executed
#'
#' This is installed as an RStudio add in and can be given a keyboard shortcut
#' through `Tools` > `Modify Keyboard Shortcuts...`
#'
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
