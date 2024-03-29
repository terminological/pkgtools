#' Fixes utf8 encoded characters in source files replaincg them with `\uXXXX`
#' 
#' @param pkg the package to scan
#' @param check output of a `devtools::check()` command ()
#' @param dry_run test changes without breaking originals.
#'
#' @return nothing
#' @export
fix_utf8_encoding = function(pkg = ".", check, dry_run = FALSE) {
  
  pkg = devtools::as.package(pkg)
  if (rlang::is_missing(check)) check = qcheck(pkg=pkg$path, quiet=TRUE)
  
  noteswarns = paste0(c(check$warnings,check$notes),collapse = "\n")
  
  nsUTF8 = noteswarns %>% 
    stringr::str_extract("non-ASCII characters:(([\\n\\r\\s]*([^\\.]+\\.R))*)",1) %>%
    stringr::str_split("[\\n\\r\\s]+") %>%
    purrr::list_c() %>%
    purrr::discard(~ is.na(.x) | .x == "")
  
  paths = fs::dir_ls(pkg$path, recurse = TRUE)
  paths = paths[fs::path_file(paths) %in% nsUTF8]
  
  tmp = paths %>% purrr::map_chr( ~ {
      content = suppressMessages(readr::read_file(.x))
      content3 = .escape_utf8(content)
      .write_safe(content3, .x, dry_run=dry_run)
    }) %>% unname()
  
  if (length(tmp) > 0)  {
    message(paste0(c("fixing UTF8 issues:",tmp), collapse = "\n\t"))
  } else {
    message("no UTB issues to fix")
  }
  
}