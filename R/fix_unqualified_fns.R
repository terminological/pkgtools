#' Fix unqualified functions in active source pane
#' 
#' Interactively find and replace unqualified, e.g. `mutate(...)` calls with
#' fully qualified `dplyr::mutate(...)` calls.
#'
#' @return nothing - called for side effects
#' @export
fix_unqualified_fns = function() {
  if (require(rstudioapi,quietly = TRUE) && require(diffobj,quietly = TRUE)) {
    context = rstudioapi::getSourceEditorContext()
    content = context$content
    path = context$path
    package_map = .package_map(path)
    tmp = .process_content(content, package_map)
    if (tmp$changed) {
      if (!.punkmode()) {
        print(diffobj::diffChr(content,tmp$new_content, interactive = FALSE, mode="sidebyside"))
        proceed = utils::askYesNo("Update editor with suggested changes?",default = TRUE)
        if (proceed) rstudioapi::setDocumentContents(text = paste0(tmp$new_content, collapse="\n"), context$id)
      } else {
        rstudioapi::setDocumentContents(text = paste0(tmp$new_content, collapse="\n"), context$id)
      }
    } else {
      message("No unqualified functions found.")
    }
  } else {
    message("Please install 'rstudioapi' and 'diffobj' packages for interactive use in Rstudio")
  }
}

#' Fix errors introduced in package creation by forgetting to qualify
#' namespaces.
#'
#' This is a code linting function and expected to be called at the console
#' during package development. It will scan the files in the current project and
#' replace unqualified references to e.g. `mutate` with ones to `dplyr::mutate`
#' etc.
#'
#' @param pkg the package
#' @param rDirectories the locations of the R code to fix (by default R scripts,
#'   and tests, but not vignettes)
#' @param dry_run by default this function will not actually do anything unless
#'   this is set to FALSE. However the dry run output can be manually compared
#'   with a diff tool to interactively accept changes.
#' @param prioritise a list of package names to pick from first
#'
#' @return nothing. called for side effects.
#' @export
fix_unqualified_fns_bulk = function(
    pkg = ".", 
    rDirectories = c(here::here("R"), here::here("tests/testthat")), 
    dry_run = FALSE, 
    prioritise = c("dplyr","rlang","stringr","forcats","ggplot2","purrr","tidyr","readr","stats","utils")
  ) {
  
  pkg = devtools::as.package(pkg)
  description_file = fs::path(pkg$path, "DESCRIPTION")
  rDirectories = rDirectories[fs::dir_exists(rDirectories)]
  packageMap2 = .package_map(pkg,prioritise)
  packages = levels(packageMap2$package)
  
  # locate source files in the directories
  files = dplyr::bind_rows(lapply(rDirectories, fs::dir_info)) %>% 
    dplyr::filter(fs::path_ext(path) %in% c("R","Rmd")) %>%
    # load all the content as a list column, make a copy
    dplyr::mutate(content.old = purrr::map(path, ~ readr::read_lines(.x))) %>%
    dplyr::mutate(content = content.old, matches=list(tibble::tibble()))
  
  #TODO: use .process_content instead
  for (packge in packages) {
    
    functions = packageMap2 %>% 
      dplyr::filter(package == packge) %>% 
      dplyr::pull(function_name)
    
    nsqualifier = sprintf("%s::",packge)
    
    if (length(functions) > 0) {
      functionNames = paste0(lapply(functions, .escape),collapse = "|")
      # match a function as the start of line or a non alphanumeric, underscore or dot
      # spaces are allowed between function name and bracket (who knew)
      # but this creates a lot of false positives.
      # functionNames = testfn
      # stringr::str_detect(c("testfn(sdf)","testfn(sdf)","+testfn(sdf)","+testfn (sdf)"), functionRegex)
      # stringr::str_detect(c("no_testfn(sdf)","another_testfn(sdf)","old.testfn(sdf)"), functionRegex)
      functionRegex = paste0("(^|[^:a-zA-Z0-9\\._])(",functionNames,")\\(")
      replacement = paste0("\\1",packge,"::\\2(")
      # c = files$content.old[[1]]
      for (i in 1:nrow(files)) {
        file = files %>% purrr::map(~ .x[[i]])
        # fix the unqualifed 
        files$content[[i]] = file$content %>% stringr::str_replace_all(functionRegex, replacement)
        # which function matched we need to know which one was matched
        tmp = stringr::str_match_all(file$content,functionRegex) %>% purrr::map(~ .x[,3]) %>%
          unlist()
        if (is.null(tmp)) tmp = character()
        # count the number of matched in a file
        tmp = tibble::tibble(name=tmp) %>% dplyr::group_by(name) %>%
          dplyr::summarise(value = dplyr::n()) %>% dplyr::mutate(pkg = packge)
        # combine this with matches from previous packages
        files$matches[[i]] = file$matches %>% dplyr::bind_rows(tmp)
      }
    }
  }
  
  
  files = files %>% dplyr::mutate(changed = purrr::map2_lgl(content.old, content, ~ any(.x!=.y)))
  # get the summary of what was matched
  tmp = files %>% dplyr::select(path,matches) %>% tidyr::unnest(matches)
  
  if(any(files$changed)) {
    if (!.punkmode()) {
      message(sum(tmp$value)," function calls missing namespaces found: ", paste0(unique(tmp$pkg), collapse = "; "))
      files %>% dplyr::filter(changed) %>% purrr::pwalk(function(path,content.old,...) message(path))
    }
    
    if (.punkmode()) {
      fixme = 1
    } else if (dry_run) {
      fixme = 3
    } else {
      fixme = utils::menu(c("Yes","No","Dry-run"), "Would you like me to fix these?")
    }
    
    dry_run = fixme==3
    
    if(fixme %in% c(1,3)) {
      
      tmp = files %>% dplyr::filter(changed) %>% 
        purrr::pmap_chr(function(path,content,...) .write_safe(content, path, dry_run=dry_run)) %>%
        unname()
      
      message(paste0(c("fixing namespace issues:",tmp), collapse = "\n\t"))

    }
    
  }
  
  suggests = pkgload::parse_deps(pkg$suggests)$name
  nsMissing = files %>% tidyr::unnest(matches) %>% dplyr::filter(!(pkg %in% c(imports,suggests))) %>% dplyr::pull(pkg) %>% unique()
  
  if(length(nsMissing) > 0) {
    if (!.punkmode()) {
      message("Your DESCRIPTION file is missing packages that are currently loaded and used in your code. These are: ",paste(nsMissing,collapse = "; "))
      desc = desc::desc(file = description_file)
      fixns = utils::menu(c("Yes","No"), title = "Would you like me to fix these?")
    } else {
      fixns = 1
    }
    
    if (fixns==1) {
      x = apply(nsMissing, desc$set_dep, type="Imports")
      .write_safe(desc, file = description_file, dry_run=dry_run)
    }
  }
  
  # TODO: format a diff output
  # tmp = diffobj::diffChr(files$content[[6]],files$content.old[[6]])
  
  if (!.punkmode()) message("Done. You may want to run some tests before deleting the backup files.")
  return(files)
}


## Utilities ----

# .package_map(getwd())
# .package_map(".")
# .package_map(rstudioapi::getSourceEditorContext()$path)
.package_map = function(
    pkg = ".", 
    prioritise = c("dplyr","rlang","stringr","forcats","ggplot2","purrr","tidyr","readr")
) {
  
  pkg = .find_package(pkg)
  if (!is.null(pkg)) {
    pkg = devtools::as.package(pkg)
    # get the dev version
    suppressWarnings(devtools::load_all(path = pkg$path,quiet = TRUE))
    # the stated imports of the current project
    imports = pkgload::parse_deps(pkg$imports)$name
    cur_package = pkg$package
  } else {
    imports = c()
    cur_package = NULL
  }
  # the currently loaded packages which will include utils, base
  loaded = (.packages())
  packages = unique(c(imports,loaded))
  
  # sort the packages first up being unqualified next being this package, and
  # base, then tidyverse if installed. The tidyverse package order. all of
  # tidyselect seems to be imported into dplyr
  tmp = unique(c(prioritise,.imports("tidyverse")))
  packages = forcats::as_factor(unique(c(cur_package,"base","utils", dplyr::intersect(packages, tmp),  packages)))
  nsqualifier = ifelse(packages %in% c(cur_package,"base"), "", sprintf("%s::",packages))
  # exclude the current ones
  # packages = packages[!packages %in% c(cur_package,"base")]
  
  packageMap = tibble::tibble(
    package = packages,
    nsqualifier = nsqualifier
  ) %>%
    dplyr::rowwise() %>%
    # dplyr::mutate(function_name = list(ls(envir = asNamespace(package)))) %>%
    dplyr::mutate(function_name = list(as.character(utils::ls.str(envir = asNamespace(as.character(package)),mode = "function")))) %>%
    tidyr::unnest(function_name) # %>%
  # dplyr::mutate( f = purrr::map2(function_name, package, function(f,p) {
  #   tryCatch(utils::getFromNamespace(f,p), error = function(e) {function(){}})
  # })) %>%
  # dplyr::mutate( generic = .isGeneric(f)) %>%
  # dplyr::select(-f)
  
  # functions from base or this package
  # these don;t need to be qualified
  ## theseFunctions = c(ls(envir = asNamespace(cur_package)),ls(asNamespace("base")))
  
  packageMap2 = packageMap %>% 
    dplyr::group_by(function_name) %>%
    # this selected a generic version of a function in one package over non
    # generic in another package. This is not important now we can prioritise.
    ## dplyr::arrange(dplyr::desc(generic),package) %>%
    # we are reverse sorting by function name to make sure generics are last.
    # this will always match current package or base first.
    dplyr::arrange(package) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    # this restricts functions to be matched by excluding base or current package
    dplyr::filter(!package %in% c(cur_package, "base")) %>%
    ## dplyr::filter(!function_name %in% theseFunctions) %>%
    # there are a few functions (like the magittr pipe) that need different
    # treatment outside of the scope of this library
    dplyr::filter(stringr::str_starts(function_name,"[a-zA-Z]")) %>%
    dplyr::filter(!is.na(function_name))
  
  return(packageMap2)
}

# .find_package(getwd())
# .find_package(".")
# .find_package(rstudioapi::getSourceEditorContext()$path)
.find_package = function(path) {
  path = fs::path_abs(fs::path_expand(path))
  if (path == fs::path_home()) {
    warning("No desription file found. Are you in a project?",call. = FALSE)
    return(NULL)
  }
  if (fs::file_exists(fs::path(path,"DESCRIPTION"))) return(path)
  .find_package(fs::path_dir(path))
}

# content = rstudioapi::getSourceEditorContext()$content
# path = rstudioapi::getSourceEditorContext()$path
# package_map = .package_map(path)
# tmp = .process_content(content, package_map)
.process_content = function(content, package_map) {
  
  packages = levels(package_map$package)
  
  matches = tibble::tibble()
  old_content = content
  for (packge in packages) {
    
    functions = package_map %>% 
      dplyr::filter(package == packge) %>% 
      dplyr::pull(function_name)
    
    nsqualifier = sprintf("%s::",packge)
    
    if (length(functions) > 0) {
      functionNames = paste0(lapply(functions, .escape),collapse = "|")
      # match a function as the start of line or a non alphanumeric, underscore or dot
      # spaces are allowed between function name and bracket (who knew)
      # but this creates a lot of false positives.
      # functionNames = testfn
      # stringr::str_detect(c("testfn(sdf)","testfn(sdf)","+testfn(sdf)","+testfn (sdf)"), functionRegex)
      # stringr::str_detect(c("no_testfn(sdf)","another_testfn(sdf)","old.testfn(sdf)"), functionRegex)
      functionRegex = paste0("(^|[^:a-zA-Z0-9\\._])(",functionNames,")\\(")
      replacement = paste0("\\1",packge,"::\\2(")
      # c = files$content.old[[1]]
      content = content %>% stringr::str_replace_all(functionRegex, replacement)
      # which function matched we need to know which one was matched
      tmp = stringr::str_match_all(content,functionRegex) %>% purrr::map(~ .x[,3]) %>%
          unlist()
      if (is.null(tmp)) tmp = character()
      
      # count the number of matched in a file
      tmp = tibble::tibble(name=tmp) %>% dplyr::group_by(name) %>%
        dplyr::summarise(value = dplyr::n()) %>% dplyr::mutate(pkg = packge)
      matches = matches %>% dplyr::bind_rows(tmp)
    }
    
  }
  
  return(list(
    new_content = content,
    changed = !identical(content, old_content),
    matches = matches
  ))
  
}

