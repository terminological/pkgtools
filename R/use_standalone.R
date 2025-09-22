#' Extended version of `use_standalone` that works with analysis projects
#'
#' `usethis::use_standalone` is a package development tool used in `r-lib` to
#' share useful functions between packages without creating a hard dependency on
#' them. This is also useful in data analysis projects where no package
#' infrastructure exists but you want to reuse common functions (e.g. plot
#' themes) between analysis projects. Developing a package containing these
#' shared functions and deploying to `CRAN` or `r-universe` is possible but it
#' is unwieldy and requires more infrastructure that needed.
#'
#' Using a standalone file we can develop these functions in a basic git
#' repository with no deployment (with or without package infrastructure), and
#' import them into an analysis project as standalone files. From a
#' reproducibility point of view this is sometimes beneficial if the functions
#' in question are fairly dynamic as the version is hard wired into the analysis
#' project.
#'
#' The use cases supported by `usethis::use_standalone` are predicated around R
#' package development but here we extend this behaviour to analysis projects
#' with dependencies managed by `renv`, or not managed at all. If an analysis
#' project is being managed by `renv` then using a standalone file will install
#' missing `renv` dependencies and snapshot the project. If no `renv` is
#' detected a check is written to the `.RProfile` file which will produce a
#' message about missing dependencies when the project is opened.
#'
#' In a non package project directives to try and `source` all standalone files
#' are added to the `.RProfile` file, so that standalone file functions are
#' immediately available.
#'
#' If this is not working you may need to set the repository to HTTP/2:
#' `git config --global http.version HTTP/2 && git push`
#'
#' @concept standalone
#'
#' @inherit usethis::use_standalone
#' @export
use_standalone = function(
  repo_spec,
  file = NULL,
  ref = NULL,
  host = NULL,
  git_dir = fs::path_home("Git")
) {
  # this may or may not be a package project:
  wd = here::here()
  fs::dir_create(fs::path(wd, "R"))
  is_pkg = fs::file_exists(fs::path(wd, "DESCRIPTION"))

  if (is.null(ref)) {
    # if no ref is given we try and find a local copy first:
    local_file = tryCatch(
      {
        # error thrown if no local repo:
        local_repo = .find_local_repo(repo_spec, git_dir)
        local_dir = local_repo$local
        if (!is.null(file)) {
          file = as_standalone_file(file)
          if (!fs::file_exists(fs::path(local_dir, "R", file))) {
            stop("local standalone file :", file, " not found in: ", local_dir)
          }
        }
        if (is.null(file)) {
          local_files = .find_local_standalones(local_dir)
          file = standalone_choose(local_files)
        }
        local_file = fs::path(local_dir, "R", file)
      },
      error = function(e) {
        message(e$message)
        message("using remote standalone from github.")
        NULL
      }
    )
  } else {
    # if a ref is given we go straight to github
    local_file = NULL
  }

  # We use a local copy if one exists and the ref is not set.
  use_local = !is.null(local_file)

  if (!use_local) {
    if (is.null(file)) {
      remote_files = .find_github_standalones(repo_spec, ref = ref, host = host)
      remote_file = standalone_choose(remote_files)
    } else {
      remote_file = as_standalone_file(file)
    }
  }

  dest_path = fs::path(wd, "R", gsub("standalone-", "import-standalone-", file))

  if (!use_local) {
    src_path = fs::path("R", remote_file)
    lines <- read_github_file(
      repo_spec,
      path = src_path,
      ref = ref,
      host = host
    )
  } else {
    src_path = fs::path("R", file)
    lines = readr::read_lines(local_file)
  }

  # Prepend a header
  lines <- c(standalone_header(repo_spec, src_path, ref, host), lines)

  # write the standalone to the R directory
  readr::write_lines(lines, file = dest_path)

  # handle inter-standalone dependencies
  # get the
  yaml = .parse_yaml(lines)
  for (dependency in yaml$dependencies) {
    use_standalone(
      repo_spec,
      dependency,
      ref = ref,
      host = host,
      git_dir = git_dir
    )
  }

  # Manage imports. This is where we depart significantly from usethis to cope
  # with the possibility we are in an analysis project.

  # get all the imports for all standalones in this project
  # this will deduplicate dependencies and pick maximum required version:
  imports <- standalone_imports(wd)

  renv_mode = fs::file_exists(fs::path(wd, "renv.lock"))

  if (is_pkg) {
    # The simple case: use usethis to manage imports
    for (i in seq_len(nrow(imports))) {
      import <- imports[i, , drop = FALSE]
      if (is.na(import$ver)) {
        ver <- NULL
      } else {
        ver <- import$ver
      }
      usethis::ui_silence(usethis::use_package(import$pkg, min_version = ver))
      if (renv_mode) renv::snapshot(prompt = FALSE)
    }
  } else if (renv_mode) {
    # we are in a non package project managed by renv

    for (i in seq_len(nrow(imports))) {
      import <- imports[i, , drop = FALSE]

      # get the version info formatted correctly for renv
      if (is.na(import$ver)) {
        # no version info in depends
        ver = "*"
        ipkgver = import$pkg
      } else {
        ver = sprintf("%s %s", import$cmp, import$ver)
        ipkgver = sprintf("%s@%s", import$pkg, import$ver)
      }

      if (
        !isTRUE(suppressWarnings(pkgload::check_dep_version(
          import$pkg,
          ver
        )))
      ) {
        # install and snapshot the package
        renv::install(ipkgver, prompt = FALSE, repos = getOption("repos"))
        renv::snapshot(prompt = FALSE)
      }
    }
  } else {
    # No DESCRIPTION, no renv
    # manage imports manually in .RProfile
    # We have a fenced block in .RProfile to manage
    rprof_path = fs::path(wd, ".Rprofile")

    if (!fs::file_exists(rprof_path)) {
      fs::file_create(rprof_path)
    }
    prof_lines = readr::read_lines(fs::path(wd, ".Rprofile"))

    standalones = fs::dir_ls(
      fs::path(wd, "R"),
      regexp = ".*import-standalone.*\\.R"
    )
    # get local path from package root.
    standalones = standalones %>% stringr::str_extract("^.*/(R/.*$)", 1)

    pkg = ver = NULL

    import_list = .df_to_list_of_lists(imports)

    prof_lines = .update_fenced_block(
      prof_lines,
      .template = rprof_import_template,
      imports = import_list,
      standalones = standalones,
      .start_glue = "# Start pkgtools dependencies ----",
      .end_glue = "# End pkgtools dependencies ----"
    )

    readr::write_lines(prof_lines, fs::path(wd, ".Rprofile"))

    source(fs::path(wd, ".Rprofile"))
  }

  invisible()
}


rprof_import_template = "
# Check dependencies:
{{#imports}}
if (!requireNamespace(\"{{{pkg}}}\")) {
  message(\"Package `{{{pver}}}` must be installed.\")
}
{{/imports}}

# Load standalones:
{{#standalones}}
try(source(\"{{{.}}}\"))
{{/standalones}}
"

#' Update or create a standalone metadata block in the Rstudio editor.
#'
#' This function helps identify package imports and maintains standalone file
#' metadata. It operates on the code in the Rstudio active window and suggests
#' changes to be made to keep the standalone file metadata up to date. It is
#' intended be used interactively.
#'
#' @concept standalone
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


#' Synchronise master version of standalone file with editor contents
#'
#' This file syncs the current content of the rstudio editor window with
#' a local master standalone file, which is assumed to be in a Git
#' directory. This allows for changes to be propagated back to the master when
#' in active development. The master copy is committed before changes.
#'
#' @param git_dir absolute or relative path to git base directory (e.g. ~/Git)
#' @concept standalone
#' @return nothing used for side effects
#' @export
sync_standalone_to_master = function(git_dir = fs::path_home("Git")) {
  context = rstudioapi::getSourceEditorContext()
  content = context$content
  path = context$path

  yaml = .parse_yaml(content)

  new_content = utils::tail(content, -(min(which(content == "# ---")) - 1))
  tmp = .find_local_repo(yaml$repo, git_dir = git_dir)

  local_repo = tmp$local
  source_path = fs::path(tmp$local, "R", yaml$file)

  if (!fs::file_exists(source_path)) {
    stop("Cannot find original standalone at:", source_path)
  }
  old_content = readr::read_lines(source_path)

  if (identical(new_content, old_content)) {
    message("No changes detected in standalone file: ", path)
  } else {
    new_content[stringr::str_starts(
      new_content,
      "# last-updated:"
    )] = sprintf("# last-updated: %s", format(Sys.Date()))

    .commit_if_needed(local_repo, sprintf("Updating %s", yaml$file))

    if (!.punkmode()) {
      new_content = merge_code(
        old_content,
        new_content
      )
    }
    readr::write_lines(new_content, source_path)
  }
}


## Utilties ----

.print_ver = function(pkg, ver = NA) {
  return(
    ifelse(is.na(ver), pkg, sprintf("%s (>= %s)", pkg, ver))
  )
}


.parse_yaml = function(lines) {
  dividers <- which(lines == "# ---")
  if (length(dividers) != 2) {
    stop("Can't find yaml metadata.")
  }
  header <- lines[dividers[[1]]:dividers[[2]]]
  header <- gsub("^# ", "", header)
  temp <- withr::local_tempfile(lines = header)
  yaml <- rmarkdown::yaml_front_matter(temp)
  return(yaml)
}

# Adapted from usethis
# returns a dataframe of imports
standalone_imports = function(wd) {
  pkg = cmp = ver = NULL

  out = dplyr::tibble(
    pkg = character(),
    cmp = character(),
    ver = character()
  )

  for (file in fs::dir_ls(
    fs::path(wd, "R"),
    regexp = ".*import-standalone.*\\.R"
  )) {
    lines = readr::read_lines(file)
    yaml = .parse_yaml(lines)
    as_chr_field <- function(field) {
      if (!is.null(field) && !is.character(field)) {
        stop("Invalid dependencies specification in ", file)
      }
      if (is.null(field)) character() else field
    }
    # deps <- as_chr_field(yaml$dependencies)
    imports <- as_chr_field(yaml$imports)
    imports <- dplyr::bind_rows(lapply(imports, parse_version))
    out = dplyr::bind_rows(out, imports)
  }

  out = out %>%
    dplyr::group_by(pkg, cmp) %>%
    dplyr::summarise(ver = max(ver), .groups = "drop") %>%
    dplyr::mutate(pver = .print_ver(pkg, ver))
  return(out)
}

# Adapted from usethis
parse_version = function(field) {
  version_regex <- "(.*) \\((.*)\\)$"
  has_ver <- grepl(version_regex, field)
  if (!has_ver) {
    return(dplyr::tibble(pkg = field, cmp = NA, ver = NA))
  }
  pkg <- sub(version_regex, "\\1", field)
  ver <- sub(version_regex, "\\2", field)
  ver <- strsplit(ver, " ")[[1]]
  if (
    !rlang::is_character(ver, n = 2) || any(is.na(ver)) || !all(nzchar(ver))
  ) {
    stop(c(
      "Can't parse version `{field}` in `imports:` field.",
      i = "Example of expected version format: `rlang (>= 1.0.0)`."
    ))
  }
  return(dplyr::tibble(pkg = pkg, cmp = ver[[1]], ver = ver[[2]]))
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
  } else {
    yaml$imports = NULL
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
#' @param path current package path
#'
#' @return a character vector of imports (which may include version info)
.local_imports = function(content, path = ".") {
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

# remote standalones: returns a vector of names
.find_github_standalones = function(
  repo_spec,
  ref = NULL,
  host = NULL,
  error_call = rlang::caller_env()
) {
  json <- gh::gh(
    "/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    ref = ref,
    .api_url = host,
    path = "R/"
  )
  names <- purrr::map_chr(json, "name")
  names <- names[grepl("^standalone-", names)]
  if (length(names) == 0) {
    cli::cli_abort(
      "No standalone files found in {repo_spec}.",
      call = error_call
    )
  }
  return(names)
}

# local standalones: returns a vector of names
.find_local_standalones = function(
  repo_path,
  error_call = rlang::caller_env()
) {
  names = fs::dir_ls(fs::path(repo_path, "R"), type = "file") %>%
    fs::path_file()
  names <- names[grepl("^standalone-", names)]
  if (length(names) == 0) {
    cli::cli_abort(
      "No standalone files found in {repo_path}.",
      call = error_call
    )
  }
  return(names)
}

#' modified copy of devtools:::standalone_choose
#'
#' @param names list of potential standalones from local or remote
#' @param error_call where to raise errors
#'
#' @returns a single filename which will match standalone-*.R
standalone_choose = function(
  names,
  error_call = rlang::caller_env()
) {
  choices <- gsub("^standalone-|.[Rr]$", "", names)

  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "`file` is absent, but must be supplied.",
        i = "Possible options are {.or {choices}}."
      ),
      call = error_call
    )
  }
  choice <- utils::menu(
    choices = choices,
    title = "Which standalone file do you want to use (0 to exit)?"
  )
  if (choice == 0) {
    cli::cli_abort("Selection cancelled", call = error_call)
  }
  names[[choice]]
}

# copy of usethis:::as_standalone_file
# sanitise filename if provided.
as_standalone_file = function(file) {
  if (is.null(file)) {
    return(NULL)
  }
  if (fs::path_ext(file) == "") {
    file <- unclass(fs::path_ext_set(file, "R"))
  }
  if (!grepl("standalone-", file)) {
    file <- paste0("standalone-", file)
  }
  file
}

# copy of usethis:::read_github_file
read_github_file = function(repo_spec, path, ref = NULL, host = NULL) {
  tf <- withr::local_tempfile()
  gh::gh(
    "/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    path = path,
    ref = ref,
    .api_url = host,
    .destfile = tf,
    .accept = "application/vnd.github.v3.raw"
  )
  readr::read_utf8(tf)
}

# copy of usethis:::standalone_header
standalone_header = function(repo_spec, path, ref = NULL, host = NULL) {
  ref_string <- ref %||% "HEAD"
  host_string <- host %||% "https://github.com"
  source_comment <- glue::glue(
    "# Source: {host_string}/{repo_spec}/blob/{ref_string}/{path}"
  )
  path_string <- fs::path_ext_remove(sub(
    "^standalone-",
    "",
    fs::path_file(path)
  ))
  ref_string <- if (is.null(ref)) {
    ""
  } else {
    glue::glue(", ref = \"{ref}\"")
  }
  host_string <- if (is.null(host) || host == "https://github.com") {
    ""
  } else {
    glue::glue(", host = \"{host}\"")
  }
  code_hint <- glue::glue(
    "pkgtools::use_standalone(\"{repo_spec}\", \"{path_string}\"{ref_string}{host_string})"
  )
  generated_comment <- glue::glue("# Generated by: {code_hint}")
  c(
    "# Standalone file: do not edit by hand",
    source_comment,
    generated_comment,
    paste0("# ", strrep("-", 72 - 2)),
    "#"
  )
}

# TODO: sync_master_to_standalone
# 1) use a local copy where it exists
# 2) prepend block like this:
# Standalone file: do not edit by hand
# Source: https://github.com/ai4ci/ggoutbreak/blob/HEAD/R/standalone-test-utils.R
# Generated by: usethis::use_standalone("ai4ci/ggoutbreak", "test-utils")

# TODO: push_standalone & pull_standalone
# gh::gh
# github details here: https://docs.github.com/en/rest/repos/contents?apiVersion=2022-11-28#create-or-update-file-contents
# would want to do this in conjunction with
# https://stackoverflow.com/questions/9506181/github-api-create-branch
