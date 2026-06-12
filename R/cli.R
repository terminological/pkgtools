#' Install pkgtools CLI tools
#'
#' Creates symlinks from `system.file("pkgtools", "*.R")` into
#' `~/.local/bin/`, allowing each exported function to be run as a command-line
#' script.
#'
#' @returns nothing
#' @concept edit
#' @export
install_cli = function() {
  tmp = fs::dir_ls(system.file(package = "pkgtools"), glob = "*.R")
  cmds = fs::path_ext_remove(fs::path_file(tmp))
  fs::dir_create(fs::path_home(".local/bin"))
  lapply(seq_along(tmp), function(i) {
    fs::link_create(tmp[[i]], fs::path_home(".local/bin", cmds[[i]]))
    fs::file_chmod(tmp[[i]], "a+x")
  })
  invisible(NULL)
}
