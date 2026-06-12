#' Update the version of a package, incrementing dev versions.
#'
#' Increments the dev component (`.8999` → `.9000`) of a package `DESCRIPTION`
#' version number. This is used by [unstable()] to trigger local reinstall.
#'
#' @param pkg the path to the package
#'
#' @return the new version as a string (e.g. `"1.0.0.9000"`)
#' @export
#' @concept usethis
bump_dev_version = function(pkg = ".") {
  pkg = devtools::as.package(pkg)
  file = fs::path(pkg$path, "DESCRIPTION")
  desc = desc::desc(file)
  v = as.character(desc$get_version())
  dev = stringr::str_extract(
    v,
    "[0-9]+\\.[0-9]+\\.[0-9]+\\.([0-9]+)",
    group = 1
  )
  if (is.na(dev)) {
    dev = "8999"
  }
  dev = as.character(as.integer(dev) + 1)
  v2 = sprintf(
    "%s.%s",
    stringr::str_extract(v, "[0-9]+\\.[0-9]+\\.[0-9]+"),
    dev
  )
  desc$set_version(v2)
  desc$write(file)
  return(v2)
}
