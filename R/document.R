#' Document package and non-package projects
#'
#' `devtools::document()` requires a DESCRIPTION file, which in general we don't
#' include in analysis projects. Sometimes we want to produce development
#' documentation for functions in analysis projects so we can describe options
#' and behaviour. `pkgtools::document` will also work in a non package project
#' to produce `.Rd` files in the `man` directory for any functions declared in
#' the `R` subfolder. It also override the default behaviour of `?` to return
#' the development documentation of matching functions in the current project.
#' 
#' @inheritParams devtools::document
#' @param project.dir the directory of the current (non package project)
#'
#' @return nothing
#' @export
document = function(project.dir = ".", roclets = NULL, quiet = FALSE) {
  
  desc_path <- fs::path_norm(fs::path(project.dir,"DESCRIPTION"))
  if (!fs::file_exists(desc_path)) {
    readr::write_lines(x = c(
      sprintf("Package: %s",fs::path_file(fs::path_abs(project.dir))),
      "Version: 0.0.0.9000",
      "Encoding: UTF-8"
    ), file = desc_path)
    devtools::document(project.dir, roclets="rd",quiet)
    unlink(desc_path)
  } else {
    devtools::document(project.dir, roclets, quiet)
  }
  
  assign("?", .help, envir = globalenv())
  
}

#TODO: devtools shims equivalent 

.get_man_dir = function(from = ".") {
  from = fs::path_abs(from)
  if (fs::path_dir(from) == from) return(NULL) # reached root
  if (fs::dir_exists(fs::path(from,"man"))) return(fs::path(from,"man"))
  else return(.get_man_dir(from = fs::path_dir(from)))
}

.topic_paths = function(man_dir = .get_man_dir()) {
  if (is.null(man_dir)) return(list())
  rd_files = fs::dir_ls(man_dir, glob="*.Rd")
  map = lapply(rd_files, function(file) {
    parsed = tools::parse_Rd(file, permissive = TRUE)
    tags = vapply(parsed, function(x) attr(x, "Rd_tag")[[1]], character(1))
    unlist(parsed[tags == "\\alias"])
  })
  return(map)
}

.find_path = function(topic) {
  map = .topic_paths()
  map = map[sapply(map, function(x) topic %in% x)]
  if (length(map) == 0) return(NULL)
  return(names(map)[[1]])
}

.help = function (e1, e2) {
  e1_expr <- substitute(e1)
  if (is.name(e1_expr)) {
    topic <- as.character(e1_expr)
    pkg <- NULL
  }
  else if (is.call(e1_expr)) {
    if (identical(e1_expr[[1]], quote(`?`))) {
      topic <- NULL
      pkg <- NULL
    }
    else if (identical(e1_expr[[1]], quote(`::`))) {
      topic <- as.character(e1_expr[[3]])
      pkg <- as.character(e1_expr[[2]])
    }
    else {
      topic <- deparse(e1_expr[[1]])
      pkg <- NULL
    }
  }
  else if (is.character(e1_expr)) {
    topic <- e1
    pkg <- NULL
  }
  else {
    cli::cli_abort("Unknown input.")
  }
  local_path = .find_path(topic)
  if (!is.null(local_path)) {
    structure(list(topic = topic, pkg = "unknown", path = local_path, 
                   stage = "render", type = getOption("help_type")), class = "dev_topic")
  }
  else if (!is.null(topic) && !is.null(pkgload::dev_topic_find(topic, pkg))) {
    pkgload::dev_help(topic, pkg)
  }
  else {
    eval(as.call(list(utils::`?`, substitute(e1), substitute(e2))))
  }
}
