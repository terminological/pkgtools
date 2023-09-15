
#' Delete backup files from the current project
#'
#' @param pkg the package to delete files from.
#'
#' @return nothing
#' @export
delete_backups = function(pkg = ".") {
  
  pkg = devtools::as.package(pkg)
  backups = fs::dir_ls(pkg$path,recurse = TRUE,all = TRUE, glob = "*.old")
  if (length(backups) > 0) {
    message("About to delete the following files:")
    lapply(backups, message)
    fixns = utils::menu(c("Yes","No"), title = "Shall I delete these (forever)?")
    if (fixns==1) {
      fs::file_delete(backups)
      message("complete.")
    }
  } else {
    message("no backups found.")
  }
  
}