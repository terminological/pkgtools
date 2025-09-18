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
#' @inherit usethis::use_standalone
#' @export
use_standalone = function(repo_spec, file = NULL, ref = NULL, host = NULL) {
  wd = here::here()
  fs::dir_create(fs::path(wd, "R"))

  # usethis will successfully fetch the files but fails to process the DESCRIPTION
  # file in an analysis project
  tmp = try(usethis::use_standalone(repo_spec, file, ref, host), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    # This is not a package project
    imports <- standalone_imports(wd)

    renv_mode = fs::file_exists(fs::path(wd, "renv.lock"))

    lines = tryCatch(
      readr::read_lines(fs::path(wd, ".Rprofile")),
      error = function(e) character()
    )

    for (i in seq_len(nrow(imports))) {
      import <- imports[i, , drop = FALSE]

      if (is.na(import$ver)) {
        # no version info in depends
        pver = "*"
        ipkgver = import$pkg
      } else {
        pver = sprintf("%s %s", import$cmp, import$ver)
        ipkgver = sprintf("%s@%s", import$pkg, import$ver)
      }

      if (renv_mode) {
        if (
          !isTRUE(suppressWarnings(pkgload::check_dep_version(
            import$pkg,
            pver
          )))
        ) {
          renv::install(ipkgver, prompt = FALSE, repos = getOption("repos"))
          renv::snapshot(prompt = FALSE)
        }
      } else {
        lines = lines[
          !stringr::str_detect(lines, sprintf("# depends on %s", import$pkg))
        ]
        lines = c(
          sprintf(
            "if (!requireNamespace(\"%s\")) message(\"Package `%s` must be installed.\") # depends on %s",
            import$pkg,
            .print_ver(import$pkg, import$ver),
            import$pkg
          ),
          lines
        )
      }
    }

    # update a .Rprofile file to include sourcing standalone files
    #TODO: what happens if these are out of sequence?
    lines = lines[!stringr::str_detect(lines, "import-standalone")]
    files = fs::dir_ls(fs::path(wd, "R"), regexp = ".*import-standalone.*\\.R")
    files = fs::path_rel(files, wd)
    lines = unique(c(lines, sprintf("try(source(\"%s\"))", files)))
    readr::write_lines(lines, fs::path(wd, ".Rprofile"))
    source(fs::path(wd, ".Rprofile"))
  } else {
    #TODO: update
  }
  invisible()
}

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
standalone_imports = function(wd) {
  out = tibble::tibble(pkg = character(), cmp = character(), ver = character())

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
    dplyr::summarise(ver = max(ver), .groups = "drop")
  return(out)
}

# Adapted from usethis
parse_version = function(field) {
  version_regex <- "(.*) \\((.*)\\)$"
  has_ver <- grepl(version_regex, field)
  if (!has_ver) {
    return(tibble::tibble(pkg = field, cmp = NA, ver = NA))
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
  return(tibble::tibble(pkg = pkg, cmp = ver[[1]], ver = ver[[2]]))
}


#' Synchronise master version of standalone file with editor contents
#'
#' This file syncs the current content of the rstudio editor window with
#' a local master standalone file, which is assumed to be in a Git
#' directory. This allows for changes to be propagated back to the master when
#' in active development. The master copy is committed before changes.
#'
#' @param git_dir absolute or relative path to git base directory (e.g. ~/Git)
#'
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

# TODO: sync_master_to_standalone
# 1) use a local copy where it exists
# 2) prepend block like this:
# Standalone file: do not edit by hand
# Source: https://github.com/ai4ci/ggoutbreak/blob/HEAD/R/standalone-test-utils.R
# Generated by: usethis::use_standalone("ai4ci/ggoutbreak", "test-utils")
# ----------------------------------------------------------------------
# and

# TODO: push_standalone & pull_standalone
# gh::gh
# github details here: https://docs.github.com/en/rest/repos/contents?apiVersion=2022-11-28#create-or-update-file-contents
# would want to do this in conjunction with
# https://stackoverflow.com/questions/9506181/github-api-create-branch
