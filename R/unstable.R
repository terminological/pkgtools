#' Reload a set of packages that are in development on the local machine
#'
#' Vignette building uses a new session. Any changes in current project or
#' dependent locally developed projects are not tested unless the packages are
#' all installed using `devtools::install_local(...)`. This causes problems when
#' developing multiple packages in parallel.
#'
#' This function assumes the `path` variable is a path to a package which is under
#' version control in a Git directory. Other dependencies to this package may
#' also be under development in sibling directories. The aim is to install the
#' current version of the target package and all locally held dependencies that
#' have changed on the local disk compared to the locally installed version.
#'
#' This function scans the current package and first order dependencies, looking
#' for local development directories for any packages imported. Looks for
#' changes in files in local development directories of package and first order
#' dependencies versus files currently installed in r-library. If it finds any
#' differences it checks if there is a version change of the package,
#' bumps the version number of the development package, and installs it locally,
#' After installation it restarts R.
#'
#' Any recent file change in development directories triggers a dev version bump
#' and local package installation. After a call to `unstable()` any dependencies
#' in your local dev environment are up to date.
#'
#' If `unstable` is called from within a non package project which is using
#' `renv` then rather than installing locally using `devtools` the package is
#' built and deployed locally in the `renv` local package directory (
#' `<proj root>/renv/local`) and installed from there. The `renv` local packages are
#' placed under version control. At the moment it is a manual job to tidy this up once the
#' development package is finalised and deployed
#'
#' @concept renv
#'
#' @param path the package local development repository path. This assumes you
#'   have all your other package code in a sibling directory, e.g. `~/Git/pkg1`,
#'   `~/Git/pkg2`
#' @inheritParams remotes::install_local
#' @inheritDotParams remotes::install_local
#' @param load_lib load the package using a library command
#'
#' @return nothing
#' @export
unstable = function(
  path = ".",
  ...,
  force = TRUE,
  upgrade = "never",
  quiet = TRUE,
  load_lib = TRUE
) {
  #TODO: 1) vectorise
  #TODO: 2) on.exit code to restart / sys.on.exit
  #TODO: 3) check running context to make sure it is in globalenv before restarting.

  renv_mode = (path != "." && fs::file_exists(fs::path(getwd(), "renv.lock")))
  pkg = devtools::as.package(path)

  git_directory = fs::path_expand_r(getOption(
    "pkgtools.git",
    fs::path_dir(pkg$path)
  ))

  localpkgs = fs::dir_ls(git_directory, type = "dir")
  imports = pkgload::parse_deps(pkg$imports)$name

  alldeps = c(pkg$package, imports)

  installed = dplyr::as_tibble(utils::installed.packages()) %>%
    dplyr::transmute(
      package = Package,
      ondiskversion = Version,
      ondiskpath = fs::path(LibPath, Package)
    )

  toload = dplyr::tibble(
    package = alldeps[alldeps %in% fs::path_file(localpkgs)]
  ) %>%
    dplyr::mutate(
      gitpath = fs::path(git_directory, package),
      gitmoddate = purrr::map_vec(gitpath, ~ .latestchange(.x)),
      gitversion = purrr::map_chr(gitpath, ~ devtools::as.package(.x)$version)
    ) %>%
    dplyr::left_join(
      installed,
      by = "package"
    ) %>%
    dplyr::mutate(
      isinstalled = !is.na(path),
      ondiskmoddate = purrr::map_vec(
        ondiskpath,
        purrr::possibly(~ .latestchange(.x), otherwise = NA)
      ),
      needsupdate = !isinstalled | gitmoddate > ondiskmoddate,
      needsbump = isinstalled & needsupdate & ondiskversion == gitversion
    ) %>%
    dplyr::filter(needsupdate)

  if (nrow(toload) == 0) {
    message("no packages to update")
    return(invisible(NULL))
  }

  out = toload %>%
    dplyr::mutate(
      newdiskversion = purrr::pmap_chr(
        .,
        function(package, gitpath, gitversion, needsbump, ...) {
          if (needsbump) {
            v2 = bump_dev_version(gitpath)
          } else {
            v2 = gitversion
          }
          message("installing `", package, "` (v", v2, ") from: ", gitpath)
          if (package != pkg$package) {
            pkgtools::install_local(
              gitpath,
              force = force,
              upgrade = upgrade,
              quiet = TRUE
            )
            devtools::reload(pkgload::inst(package), quiet = TRUE)
          }
          return(v2)
        },
        .progress = "installing development packages"
      )
    ) %>%
    dplyr::select(
      package,
      gitpath,
      gitmoddate,
      version = newdiskversion,
      old_version = gitversion
    )

  # update imports to include new version info.
  descpath = fs::path(pkg$path, "DESCRIPTION")
  desc = desc::desc(file = descpath)

  vchange = out %>% dplyr::filter(version != old_version)
  if (nrow(vchange) > 0) {
    deps = dplyr::bind_rows(
      desc$get_deps() %>% dplyr::anti_join(vchange, by = "package"),
      vchange %>%
        dplyr::transmute(
          type = "Imports",
          package = package,
          version = sprintf(">= %s", version)
        )
    ) %>%
      dplyr::filter(package != pkg$package)
    desc$set_deps(deps)
    desc$write(descpath)
  }

  # install the main package
  pkgtools::install_local(pkg$path)

  # if (pkg$package != "pkgutils") {
  #   devtools::reload(pkgload::inst(pkg$package), quiet=TRUE)
  # } else {
  # try to prevent documentation db corruption
  # devtools::load_all(pkg$path, quiet=TRUE)
  # }

  pkgload::unregister(pkg$package)
  # restart R before loading
  # TODO: check https://henrikbengtsson.github.io/startup/reference/restart.html
  if (interactive()) {
    if (load_lib) {
      tmp = unique(c(rev(.packages()), pkg$package))
    } else {
      tmp = rev(.packages())
    }
    tmp = paste0(
      lapply(
        tmp,
        sprintf,
        fmt = "require(\"%s\", character.only=TRUE, quietly=TRUE)"
      ),
      collapse = "\n"
    )
    try(
      {
        rstudioapi::restartSession(command = tmp)
      },
      silent = TRUE
    )
  }
  return(invisible(out))
}
