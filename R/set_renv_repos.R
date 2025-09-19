#' Adds new repositories to the beginning of an `renv` lockfile
#'
#' Sets custom repositories (e.g. r-universe repositories) in a `renv`
#' lockfile to override CRAN repositories. This is a persistent change can be
#' undone by manual editing of the lockfile.
#'
#' @concept renv
#'
#' @param ... a named list of repository urls
#' @param .wd the working directory (defaults to `here::here()`)
#'
#' @return nothing
#' @export
set_renv_repos = function(..., .wd = here::here()) {
  repos = rlang::list2(...)
  lockfile = fs::path(.wd, "renv.lock")
  if (!fs::file_exists(lockfile)) {
    stop("No lock file found at ", lockfile)
  }
  tmp = jsonlite::read_json(lockfile)
  for (n in rev(names(repos))) {
    found = FALSE
    for (i in seq_along(tmp$R$Repositories)) {
      if (tmp$R$Repositories[[i]]$Name == n) {
        tmp$R$Repositories[[i]]$URL = repos[[n]]
        found = TRUE
      }
    }
    if (!found) {
      tmp$R$Repositories = c(
        list(list(Name = n, URL = repos[[n]])),
        tmp$R$Repositories
      )
    }
  }
  jsonlite::write_json(tmp, path = lockfile, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}
