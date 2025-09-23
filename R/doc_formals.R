#' Document default formal parameters in a function
#'
#' this is intended to be used in a Roxygen tag to document the
#' default value or allowable values of a function
#'
#' @param fn a function in the current package
#' @param param a parameter (usually the same as the param block)
#'
#' @return a formatted string for inclusion in a Roxygen block
#' @concept code
#'
#' @keywords internal
#' @export
#'
#' @examples
#' #' A test function
#' #'
#' #' @@param arg a set of descriptions: `r doc_formals(test_fn, arg)`
#' #' @@param def a set of descriptions: `r doc_formals(test_fn, def)`
#' #' @@param ghi a set of descriptions: `r doc_formals(test_fn, ghi)`
#' #'
#' #' @@ return nothing
#' test_fn = function(
#'     arg = c("option one","option two","option three"),
#'     def = 123,
#'     ghi = def*2
#' ) {
#'   arg = match.arg(arg)
#' }
#'
#' # @@param arg a set of descriptions: `r doc_formals(test_fn, arg)`
#'
#' doc_formals(test_fn, arg)
#' doc_formals(test_fn, def)
#' doc_formals(test_fn, geh)
doc_formals = function(
  fn,
  param = NULL,
  dname = deparse(substitute(param))
) {
  # param = rlang::as_label(rlang::ensym(param))

  one_of = as.character(body(fn)) %>%
    stringr::str_detect(stringr::fixed(sprintf("match.arg(%s)", dname))) %>%
    any()

  .call_x = formals(fn)
  .call_x = .call_x[[dname]]
  if (missing(.call_x)) {
    return("")
  }
  src = format(.call_x)
  x = try(eval(.call_x, rlang::fn_env(fn)), silent = TRUE)
  if (inherits(x, "try-error")) {
    # couldnt evaluate x - just return the formatted call
    return(sprintf("(default \\code{`%s`})", src))
  } else if (is.atomic(x) && one_of) {
    # We are expecting a vector.
    x = paste0("`", format(x), "`")
    return(sprintf(
      "(default \\code{%s}; allowed: \\code{%s})",
      x[[1]],
      paste0(x, collapse = ",")
    ))
  } else {
    # maybe a single atomic maybe something else
    # we can't risk returning the value, but we do know the type.
    return(sprintf("(default \\code{`%s`} [%s])", src, class(x)[[1]]))
  }
}
