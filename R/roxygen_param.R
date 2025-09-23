#' Parser for `@dparam` tags
#'
#' Replaces the default behaviour of a `@param` tag and documents default
#' parameters automatically.
#'
#' @inheritParams roxygen2::roxy_tag_parse
#' @concept roxy
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @return a `roxy_tag` object with the `val` field set to the parsed value
#' @export
#' @examples
#' # This provides support to `roxygen2` and only gets executed in the context
#' # of `devtools::document()`. There is no interactive use of this function.
roxy_tag_parse.roxy_tag_dparam = function(x) {
  roxygen2::tag_two_part(x, "an argument name", "a description")
}

#' Support for `@dparam` tags
#'
#' Replaces the default behaviour of a `@param` tag and documents default
#' parameters automatically.
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @importFrom roxygen2 roxy_tag_rd
#' @concept roxy
#' @return an `roxygen2::rd_section` (see `roxygen2` documentation)
#' @export
#'
#' @examples
#' # An example function definition:
#' fn_definition <- "
#' #' This is a title
#' #'
#' #' This is the description.
#' #'
#' #' @dparam x this parameter specifies a default
#' #'
#' f <- function(x = c(\"hello\",\"world\")) {
#'   return(\"test\")
#' }
#' "
#'
#' # For this example we manually parse the function specification in `fn_definition`
#' # creating a .Rd block - normally this is done by `roxygen2` which then
#' # writes this to an .Rd file. This function is not intended to be used
#' # outside of a call to `devtools::document`.
#'
#' tmp = roxygen2::parse_text(fn_definition)
#' print(tmp[[1]])
#'
roxy_tag_rd.roxy_tag_dparam = function(x, base_path, env) {
  dname = x$val$name
  desc = x$val$description
  block = .search_call_stack(.class = "roxy_block")
  fn = block$object$value
  out = doc_formals(fn, dname = dname)
  out = sprintf("%s %s", desc, out)
  names(out) = dname
  roxygen2::rd_section("param", out)
}
