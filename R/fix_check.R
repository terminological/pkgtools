fix_check = function(pkg=".") {
  
  path = devtools::as.package(pkg)$path
  
  if (nrow(gert::git_status(repo = path)) > 0 && !.punkmode()) {
    fixns = utils::menu(c("Yes","No"), title = "Current changes are not committed. Are you sure?")
    if (fixns != 1) {
      message("Cancelled by user.")
      resturn(invisible(NULL))
    }
  } 
  
  fix_unqualified_fns(pkg,dry_run = FALSE)
  check = qcheck(quiet=TRUE)
  fix_non_standard_files(pkg, check)
  fix_global_variables(pkg, check)
  fix_utf8_encoding(pkg,check)
  fix_dependencies(pkg,check)
  
  message("Fixing package issues complete.")
  
}