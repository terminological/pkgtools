
#' Document default formal parameters in a function
#' 
#' this is intended to be used in a Roxygen tag to document the 
#' default value or allowable values of a function
#'
#' @param fn a function in the current package
#' @param param a parameter (usually the same as the param block)
#' @param one_of decorate the string with a "default"/"one of" depending on type
#'
#' @return a formatted string for inclusion in a Roxygen block
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
doc_formals = function(fn, param, one_of = TRUE) {
  # TODO: implement this as a Roxygen tag?
  param = rlang::as_label(rlang::ensym(param))
  x = formals(fn)
  x = x[[param]]
  if (is.call(x)) x = tryCatch(eval(x), error = function(e) format(x))
  x = sprintf("`%s`",as.character(x))
  if (one_of) {
    if (length(x) > 1) {
      return(sprintf("one of: %s; default %s",paste0(x,collapse=", "),x[[1]]))
    } else {
      return(sprintf("default %s",x))
    }
  } else {
    return(paste0(x,collapse=", "))
  }
}