
#' Reload a set of packages that are on the local machine
#' 
#' Vignette building uses a new session. Any changes in current project or
#' dependent projects are not tested unless the packages are all installed using
#' `devtools::install_local(...)`. This causes problems when developing multiple
#' packages in parallel.
#' 
#' This function scans the current package and first order dependencies, looking
#' for local development directories for any packages imported. Looks for
#' changes in files in local development directories of package and first order
#' dependencies versus files currently installed in r library.
#' 
#' Any recent file change in development directories triggers a dev version bump
#' and local package installation. After a call to `unstable()` any dependencies
#' in your local dev environment are up to date.
#' 
#' @param path the package local development repository path. This assumes you 
#'   have all your other packages code in a sibling directory
#' @inheritParams remotes::install_local
#' @inheritDotParams remotes::install_local
#'
#' @return nothing
#' @export
unstable = function(path = ".", ..., force = TRUE, upgrade = "never", quiet=TRUE) {
  
  pkg = devtools::as.package(path)
  
  git_directory = fs::path_expand_r(getOption("pkgtools.git", fs::path_dir(pkg$path)))
  
  localpkgs = fs::dir_ls(git_directory, type="dir")
  imports = pkgload::parse_deps(pkg$imports)$name
  
  alldeps = c(pkg$package,imports)
  
  installed = tibble::as_tibble(utils::installed.packages()) %>%
    dplyr::transmute(
      package = Package, ondiskversion = Version, ondiskpath = fs::path(LibPath,Package))
  
  toload = tibble::tibble(
    package = alldeps[alldeps %in% fs::path_file(localpkgs)]
  ) %>% dplyr::mutate(
    gitpath = fs::path(git_directory, package),
    gitmoddate = purrr::map_vec(gitpath, ~ .latestchange(.x)),
    gitversion = purrr::map_chr(gitpath, ~devtools::as.package(.x)$version)
  ) %>% dplyr::left_join(
    installed, by="package"
  ) %>% dplyr::mutate(
    isinstalled = !is.na(path),
    ondiskmoddate = purrr::map_vec(ondiskpath, purrr::possibly(~ .latestchange(.x),otherwise=NA)),
    needsupdate = !isinstalled | gitmoddate > ondiskmoddate,
    needsbump = isinstalled & needsupdate & ondiskversion == gitversion
  ) %>% dplyr::filter(needsupdate)
  
  if (nrow(toload) == 0) {
    message("no packages to update")
    return(invisible(NULL))
  }
  
  out = toload %>% dplyr::mutate(
    newdiskversion = purrr::pmap_chr(., function(package, gitpath, gitversion, needsbump, ...) {
      if (needsbump)
        v2 = .bump_dev_version(gitpath)
      else 
        v2 = gitversion
      message("installing `",package,"` (v",v2,") from: ",gitpath)
      if (package != pkg$package) {
        devtools::document(gitpath,quiet = TRUE)
        remotes::install_local(gitpath, force = force, upgrade = upgrade, quiet=TRUE)
        devtools::reload(pkgload::inst(package), quiet=TRUE)
      }
      return(v2)
    }, .progress = "installing development packages")
  ) %>% dplyr::select(
    package, gitpath, gitmoddate, version = newdiskversion, old_version = gitversion
  )
  
  # update imports to include new version info.
  descpath = fs::path(pkg$path,"DESCRIPTION")
  desc = desc::desc(file=descpath)
  
  vchange = out %>% dplyr::filter(version != old_version)
  if (nrow(vchange) > 0) {
    deps = dplyr::bind_rows(
      desc$get_deps() %>% dplyr::anti_join(vchange, by="package"),
      vchange %>% dplyr::transmute(type = "Imports", package=package, version = sprintf(">= %s",version))
    ) %>% dplyr::filter(package != pkg$package)
    desc$set_deps(deps)
    desc$write(descpath)
  }
  
  # install the main package
  devtools::document(pkg$path)
  remotes::install_local(pkg$path, force = force, upgrade = upgrade, quiet=TRUE)
  # if (pkg$package != "pkgutils") {
  #   devtools::reload(pkgload::inst(pkg$package), quiet=TRUE)
  # } else {
    # try to prevent documentation db corruption
  # devtools::load_all(pkg$path, quiet=TRUE)
  # }
  pkgload::unregister(pkg$package)
  library(pkg$package, character.only =TRUE)
  
  return(invisible(out))
}

