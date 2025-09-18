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
