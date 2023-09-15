
#' Adds non standard and hidden files to the .`Rbuildignore` file
#'
#' @param pkg the package location
#' @param check the results of a `devtools::check`
#'
#' @return nothing
#' @export
fix_non_standard_files = function(pkg = ".", check = qcheck(quiet=TRUE)) {
  pkg = devtools::as.package(pkg)
  
  buildignore_path = fs::path(pkg$path, ".Rbuildignore")
  
  noteswarns = paste0(c(check$warnings,check$notes),collapse = "\n")
  
  nsFiles = noteswarns %>% 
    stringr::str_extract("found at top level:(([\\n\\r\\s]*(\u2018[^\u2019]+\u2019)+)*)",1) %>%
    stringr::str_match_all("\u2018([^\u2019]+)\u2019") %>% 
    purrr::map(~ .x[,2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)
  
  hiddenFiles = noteswarns %>% 
    stringr::str_extract("hidden files and directories:([\\s\\S]*?)These were most likely",1) %>%
    stringr::str_match_all("\\s*([^\\n]+)\\n") %>% 
    purrr::map(~ .x[,2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)
  
  bi = tryCatch(readr::read_lines(buildignore_path), error = function(e) character())
  bi = unique(c(bi,
    sprintf("^%s$",.escape(hiddenFiles)),
    sprintf("^%s$",.escape(nsFiles))
  ))
  
  .write_safe(bi, buildignore_path)
  invisible(NULL)
}