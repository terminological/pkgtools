
#' Adds non standard and hidden files to the .`Rbuildignore` file
#'
#' @param pkg the package location
#' @param check the results of a `devtools::check`
#'
#' @return nothing
#' @export
fix_global_variables = function(pkg = ".", check = qcheck(quiet=TRUE)) {
  pkg = devtools::as.package(pkg)
  
  globals_path = fs::path(pkg$path, "R/globals.R")
  
  noteswarns = paste0(c(check$warnings,check$notes),collapse = "\n")
  
  check_glob = noteswarns %>% 
    stringr::str_match_all("for global variable[\\n\\r\\s]*\u2018([^\u2019]+)\u2019") %>%
    purrr::map(~ .x[,2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)
  
  gl = tryCatch(readr::read_file(globals_path), error = function(e) "utils::globalVariables(c())\n")
  exist = gl %>% stringr::str_extract("globalVariables\\((c\\([\\s\\S]*?\\))\\)",1)
  cur_glob = eval(parse(text= exist))
  
  new_glob = sort(unique(c(cur_glob,check_glob)))
  form_glob = sprintf("globalVariables(c(\n\t%s\n))",paste0('"',new_glob,'"',collapse = ",\n\t"))
  
  gl2 = gl %>% stringr::str_replace("globalVariables\\(c\\([\\s\\S]*?\\)\\)", form_glob)
  
  .write_safe(gl2, globals_path)
  invisible(NULL)
}