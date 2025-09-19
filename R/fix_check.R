#' Fix common check errors
#'
#' This function runs a set of common package development errors. It will create
#' a commit before running checks and fixing unqualified functions, excluding
#' non standard files, creating global variables, fixng utf8 encoding issues
#' and ensuring all dependencies line up.
#'
#' @concept usethis
#'
#' @param pkg the package to fix
#'
#' @returns nothing
#' @export
fix_check = function(pkg = ".") {
  path = devtools::as.package(pkg)$path

  .commit_if_needed(path, "pre automated fixes")

  fix_unqualified_fns_bulk(pkg)
  check = qcheck(pkg = pkg, quiet = TRUE)
  fix_non_standard_files(pkg, check)
  fix_global_variables(pkg, check)
  fix_utf8_encoding(pkg, check)
  fix_dependencies(pkg, check)

  message("Fixing package issues complete.")
}
