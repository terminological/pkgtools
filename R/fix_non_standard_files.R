#' Adds non standard and hidden files to the .`Rbuildignore` file
#'
#' @param pkg the package location
#' @param check the results of a `devtools::check` command
#'   (will check automatically if not provided)
#'
#' @concept usethis
#' @return nothing
#' @export
fix_non_standard_files = function(pkg = ".", check) {
  pkg = devtools::as.package(pkg)
  if (rlang::is_missing(check)) {
    check = qcheck(pkg = pkg$path, quiet = TRUE)
  }

  buildignore_path = fs::path(pkg$path, ".Rbuildignore")

  noteswarns = paste0(c(check$warnings, check$notes), collapse = "\n")

  nsFiles = noteswarns %>%
    stringr::str_extract(
      "found at top level:(([\\n\\r\\s]*(\u2018[^\u2019]+\u2019)+)*)",
      1
    ) %>%
    stringr::str_match_all("\u2018([^\u2019]+)\u2019") %>%
    purrr::map(~ .x[, 2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)

  hiddenFiles = noteswarns %>%
    stringr::str_extract(
      "hidden files and directories:([\\s\\S]*?)These were most likely",
      1
    ) %>%
    stringr::str_match_all("\\s*([^\\n]+)\\n") %>%
    purrr::map(~ .x[, 2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)

  bi = tryCatch(readr::read_lines(buildignore_path), error = function(e) {
    character()
  })
  bi2 = unique(c(
    bi,
    sprintf("^%s$", .escape(hiddenFiles)),
    sprintf("^%s$", .escape(nsFiles))
  ))

  if (!identical(bi2, bi)) {
    .write_safe(bi2, buildignore_path)
  } else {
    message("Complete: no non standard files to fix.")
  }
  invisible(NULL)
}
