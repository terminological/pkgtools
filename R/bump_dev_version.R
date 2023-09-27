#' Update the version of a package, incrementing dev versions.
#' 
#' This makes no checks and accepts no responsibility. No backups are made.
#' 
#' @param pkg the path to the package 
#'
#' @return the new version
#' @export
bump_dev_version = function(pkg = ".") {
  pkg = devtools::as.package(pkg)
  file = fs::path(pkg$path,"DESCRIPTION")
  desc = desc::desc(file)
  v = as.character(desc$get_version())
  dev = stringr::str_extract(v,"[0-9]+\\.[0-9]+\\.[0-9]+\\.([0-9]+)",group = 1)
  if (is.na(dev)) dev="8999"
  dev = as.character(as.integer(dev)+1)
  v2 = sprintf("%s.%s",stringr::str_extract(v, "[0-9]+\\.[0-9]+\\.[0-9]+"), dev)
  desc$set_version(v2)
  desc$write(file)
  return(v2)
}