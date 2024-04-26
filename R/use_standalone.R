

#' Extended version of `use_standalone` that works with `renv` projects
#' 
#' `usethis::use_standalone` is a package development tool used in `r-lib`
#' to share useful functions between packages without creating a hard dependency
#' on them. This is also useful in data analysis projects where no package 
#' infrastructure exists but you want to reuse common functions (e.g. plot themes)
#' between analysis projects. Developing a package containing these
#' shared functions and deploying to `CRAN` or `r-universe` but it is unwieldy and 
#' requires more infrastructure that needed.
#' 
#' Using a standalone file we can develop these functions in a basic git
#' repository with no deployment (with or without package infrastructure), and
#' import them into a project as standalone files. From a reproducibility point
#' of view this is sometimes beneficial as the version is hard wired into the
#' analysis project.
#' 
#' The use cases supported by `usethis` are predicated around R package
#' development but here we extend this behaviour to analysis projects with
#' dependencies managed by `renv`. 
#' 
#' @inherit usethis::use_standalone
#' @export
use_standalone = function (repo_spec, file = NULL, ref = NULL, host = NULL) {
  wd = here::here()
  fs::dir_create(fs::path(wd,"R"))
  tmp = try(usethis::use_standalone(repo_spec, file, ref, host),silent = TRUE)
  if (inherits(tmp,"try-error")) {
    
    # This is not a package project
    imports <- standalone_imports(wd)
    
    renv_mode =  fs::file_exists(fs::path(wd,"renv.lock"))
    if (renv_mode) {
      
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
        
        if (!isTRUE(suppressWarnings(pkgload::check_dep_version(import$pkg,pver)))) {
          renv::install(ipkgver,prompt = FALSE,repos = getOption("repos"))
          renv::snapshot(prompt = FALSE)
        }
        
      }
      
      # update a .Rprofile file (which will exist in an renv project) to include
      # standalone files
      lines = readr::read_lines(fs::path(wd,".Rprofile"))
      lines = lines[!stringr::str_detect(lines,"import-standalone")]
      files = fs::dir_ls(fs::path(wd,"R"),regexp = ".*import-standalone.*\\.R")
      files = fs::path_rel(files, wd)
      lines = unique(c(lines, sprintf("try(source(\"%s\"))",files)))
      readr::write_lines(lines, fs::path(wd,".Rprofile"))
      source(fs::path(wd,".Rprofile"))
      
    } else {
      if (nrow(imports)>0) {
        message("No package DESCRIPTION and no renv setup detected.",appendLF = TRUE)
        message("Installation of the following packages must be verified:",appendLF = TRUE)
        message(paste0(.print_ver(imports$pkg, imports$ver),collapse = "\n"),appendLF = TRUE)
      }
    }
    
  }
  invisible()
}

.print_ver = function(pkg, ver=NA) {
  return(
    ifelse(is.na(ver), pkg, sprintf("%s (>= %s)",pkg, ver))
  )
}

# Adapted from usethis
standalone_imports = function (wd) {
  out = tibble::tibble(pkg = character(), cmp = character(), ver = character())
  
  for (file in fs::dir_ls(fs::path(wd,"R"),regexp = ".*import-standalone.*\\.R")) {
    lines = readr::read_lines(file)
    dividers <- which(lines == "# ---")
    if (length(dividers) != 2) {
      stop("Can't find yaml metadata in ", file)
    }
    header <- lines[dividers[[1]]:dividers[[2]]]
    header <- gsub("^# ", "", header)
    temp <- withr::local_tempfile(lines = header)
    yaml <- rmarkdown::yaml_front_matter(temp)
    as_chr_field <- function(field) {
      if (!is.null(field) && !is.character(field)) {
        stop("Invalid dependencies specification in ",file)
      }
      if (is.null(field)) character() else field
    }
    # deps <- as_chr_field(yaml$dependencies)
    imports <- as_chr_field(yaml$imports)
    imports <- dplyr::bind_rows(lapply(imports, parse_version))
    out = dplyr::bind_rows(out, imports) 
  }
  
  out = out %>% dplyr::group_by(pkg,cmp) %>% dplyr::summarise(ver = max(ver), .groups="drop")
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
  if (!rlang::is_character(ver, n = 2) || any(is.na(ver)) || !all(nzchar(ver))) {
    stop(c("Can't parse version `{field}` in `imports:` field.", 
                     i = "Example of expected version format: `rlang (>= 1.0.0)`."))
  }
  return(tibble::tibble(pkg = pkg, cmp = ver[[1]], ver = ver[[2]]))
}