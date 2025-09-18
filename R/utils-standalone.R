#' Update or create a standalone metadata block in the Rstudio editor.
#'
#' This function helps identify package imports and maintains standalone file
#' metadata. It operates on the code in the Rstudio active window and suggests
#' changes to be made to keep the standalone file metadata up to date. It is
#' intended be used interactively.
#'
#' @param repo the github repository e.g. `organisation/project`
#' @param license a license URL (defaults to `http:://unlicense/org`)
#' @param dependencies a optional list of standalone filenames in the same repository
#' @export
#'
#' @return nothing, updates the RStudio editor content
update_standalone = function(
  repo = NULL,
  license = NULL,
  dependencies = NULL
) {
  context = rstudioapi::getSourceEditorContext()
  content = context$content
  path = context$path
  remote = .find_remote_repo(context$path)
  if (is.null(repo)) {
    repo = remote$remote
  }
  block = which(stringr::str_detect(content, "^#\\s---$"))

  yml = .merge_standalone(
    content,
    path,
    repo = repo,
    license = license,
    dependencies = dependencies
  )
  yml = c(
    "# ---",
    paste0("# ", unlist(stringr::str_split(trimws(yaml::as.yaml(yml)), "\\n"))),
    "# ---"
  )

  if (length(block) == 0) {
    new_content = c(yml, "", content)
  } else if (length(block) == 2) {
    new_content = c(
      if (block[1] > 1) content[1:(block[1] - 1)] else character(),
      yml,
      content[(block[2] + 1):length(content)]
    )
  } else {
    stop("malformed standalone metadata block", call. = FALSE)
  }

  if (!identical(content, new_content)) {
    new_content = merge_code(
      old = content,
      new = new_content,
      value = new_content
    )
    rstudioapi::setDocumentContents(
      text = paste0(new_content, collapse = "\n"),
      context$id
    )
  } else {
    message("no updates needed.")
  }
}


#' Parse standalone yaml block
#'
#' @param content a character vector of the file contents
#' @param path the file path
#' @noRd
#'
#' @return an R list (empty if no metadata found)
#' @examples
#' context = rstudioapi::getSourceEditorContext()
#' content = context$content
#' path = context$path
#' .parse_standalone(content, path)
.parse_standalone = function(content, path) {
  block = which(stringr::str_detect(content, "^#\\s---$"))
  if (length(block) == 0) {
    warning("no standalone metadata found", call. = FALSE)
    return(list())
    yaml = list()
  } else if (length(block) == 2) {
    yaml = content[(block[1] + 1):(block[2] - 1)]
    yaml = stringr::str_replace(yaml, "^# ", "")
    yaml = yaml::read_yaml(text = yaml)
  } else {
    stop("malformed standalone metadata block", call. = FALSE)
  }
  return(yaml)
}


#' Get standalone yaml metadata and merge changes.
#'
#' @param content R code source
#' @param path the file path
#' @param repo the github repository e.g. `organisation/project`
#' @param license a license URL (defaults to `http:://unlicense/org`)
#' @param dependencies a optional list of standalone filenames in the same repository
#' @param imports a list of R packages (by default determined from `content`)
#' @noRd
#'
#' @return a yaml metadata block
#' @examples
#' context = rstudioapi::getSourceEditorContext()
#' content = context$content
#' path = context$path
#' yml = .merge_standalone(content, path, repo="terminological/pkgtools")
.merge_standalone = function(
  content,
  path = NULL,
  repo = NULL,
  license = NULL,
  dependencies = NULL,
  imports = .local_imports(content, path)
) {
  if (!is.null(path) && fs::file_exists(path)) {
    saved_version = readr::read_lines(path)
    updated = !identical(saved_version, content)
    yaml = .parse_standalone(content, path)
    yaml$file = fs::path_file(path)
  } else {
    yaml = list(
      file = "<ADD FILE PATH DETAILS>"
    )
    updated = TRUE
  }

  yaml$file = stringr::str_remove(yaml$file, "import-")
  if (updated || is.null(yaml$`last-updated`)) {
    yaml$`last-updated` = as.character(Sys.Date())
  }
  if (!is.null(repo)) {
    yaml$repo = repo
  }
  if (!is.null(licence)) {
    yaml$license = license
  }
  if (is.null(yaml$license)) {
    yaml$license = "https://unlicense.org"
  }
  if (is.null(yaml$repo)) {
    yaml$repo = "<ADD REPO DETAILS>"
  }
  yaml$dependencies = sort(unique(c(yaml$dependencies, dependencies)))
  yaml$imports = unlist(unique(c(yaml$imports, imports)))
  yaml$imports = yaml$imports[yaml$imports != ""]
  if (length(yaml$imports) > 0) {
    yaml$imports = sort(yaml$imports)
  }
  yaml = yaml[intersect(
    c("repo", "file", "last-updated", "license", "dependencies", "imports"),
    names(yaml)
  )]
  return(yaml)
}

#' Identify the package dependencies in R code.
#' @noRd
#'
#' @param content a vector of R source code lines.
#'
#' @return a character vector of imports (which may include version info)
#' @export
.local_imports = function(content, path) {
  package_map = .package_map(path)
  pkgs = stringr::str_match_all(content, "([a-zA-Z0-9]+)::([a-zA-Z0-9]+)") %>%
    lapply(function(x) x[, 2]) %>%
    unlist() %>%
    unique()
  tmp = .process_content(content, package_map)
  if (length(tmp$matches$pkg) > 0) {
    message(
      "unqualified functions found. Please run `fix_unqualified_fns()` to resolve.",
      call. = FALSE
    )
  }
  return(sort(unique(c(pkgs, tmp$matches$pkg))))
}
