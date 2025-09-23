#' This is a title
#'
#' Default values
#'
#' @dparam x this parameter specifies a default
#' @dparam y this parameter has a default call
#' @dparam z this parameter has a dataframe input
#' @dparam a this parameter has no default
#' @dparam b a vector of characters
#'
test_fn <- function(
  x = c("hello", "world"),
  y = toupper(x),
  z = iris,
  a,
  b = c("one", "two")
) {
  x = match.arg(x)
  return(x)
}
