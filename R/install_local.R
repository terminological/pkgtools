
#' Install package locally using `renv` if available.
#' 
#' `devtools::install_local` does not play well with `renv` in this version
#' of `install_local` we intercept installation of locally developed packages 
#' when we are in a `renv` managed project and installing a local dependency, it builds a source project into
#' `renv` cellar and installs it from there. This allows a copy of a locally developed package to 
#' be deployed with the `renv` managed analysis project without specifically being
#' deployed to `CRAN` or `r-universe`.
#' 
#' If installed locally for a non-`renv` project (e.g. a package development) 
#' the usual behaviour applies to version management. Installation of new versions
#' of the project will happen when the package is released and then installed from the
#' release location (e.g. github, cran, r-universe).
#' 
#' If a locally developed package is deployed to an `renv` project once it is released onto a valid distribution
#' platform e.g. CRAN, r-universe or github, we will want to use that version in
#' out `renv`. This we can do using the `rebuild = TRUE` option of `renv::install`, e.g.: 
#' `renv::install(...pkg name/github..., repo = ...r-universe?..., rebuild = TRUE)`
#' followed by a `renv::snapshot()` to update the lock file. The locally built 
#' package version will remain in the `<projroot>/renv/local` cellar until removed
#' by hand.
#'
#' @param wd the project root directory of the current project (defaults to `here::here()`)
#' @inherit remotes::install_local
#' @export
install_local = function(path = ".", ..., force = TRUE, upgrade = "never", quiet=TRUE, wd = here::here()) {
  
  renv_mode =  (path != "." && fs::file_exists(fs::path(wd,"renv.lock")))
  pkg = devtools::as.package(path)
  devtools::document(pkg$path,quiet = TRUE)
  
  if (renv_mode) {
    
    renv_local = fs::path(wd, "renv", "local")
    local_path = fs::path(renv_local, pkg$package,sprintf("%s_%s.tar.gz", pkg$package, pkg$version))
    fs::dir_create(fs::path_dir(local_path))                
    devtools::build(pkg$path, path=local_path)
    renv::install(sprintf("%s@%s",pkg$package, pkg$version),prompt = FALSE)
    
    readr::read_lines(fs::path(wd,"renv",".gitignore")) %>% purrr::discard(~ .x == "local/") %>% readr::write_lines(fs::path(wd,"renv",".gitignore"))
    
  } else {
    
    remotes::install_local(pkg$path, force = force, upgrade = upgrade, quiet=TRUE, ...)
    
  }
  
}