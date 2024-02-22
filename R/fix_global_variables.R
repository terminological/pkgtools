
#' Adds global variables identified at `R CMD check`` to a `globals.R` file
#'
#' @param pkg the package location
#' @param check the results of a `devtools::check`
#'
#' @return nothing
#' @export
fix_global_variables = function(pkg = ".", check) {
  pkg = devtools::as.package(pkg)
  if (rlang::is_missing(check)) check = qcheck(pkg=pkg$path, quiet=TRUE)
  
  globals_path = fs::path(pkg$path, "R/globals.R")
  
  noteswarns = paste0(c(check$warnings,check$notes),collapse = "\n")
  
  check_glob = noteswarns %>% 
    stringr::str_match_all("for global variable[\\n\\r\\s]*\u2018([^\u2019]+)\u2019") %>%
    purrr::map(~ .x[,2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)
  
  gl = tryCatch(readr::read_file(globals_path), error = function(e) "utils::globalVariables(c())\n")
  exist = gl %>% stringr::str_extract("globalVariables\\([\\s\\n\\r]*(c\\([\\s\\S]*?\\))[\\s\\n\\r]*\\)",1)
  cur_glob = eval(parse(text= exist))
  
  new_glob = sort(unique(c(cur_glob,check_glob)))
  form_glob = sprintf("globalVariables(c(\n\t%s\n))",paste0('"',new_glob,'"',collapse = ",\n\t"))
  
  gl2 = gl %>% stringr::str_replace("globalVariables\\([\\s\\n\\r]*c\\([\\s\\S]*?\\)[\\s\\n\\r]*\\)", form_glob)
  
  if (!identical(gl,gl2)) {
    .write_safe(gl2, globals_path)
  } else {
    message("complete. No missing global variables to fix.")
  }
  invisible(NULL)
}