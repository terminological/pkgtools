# Regex find and replace over the whole package codebase.
# What could go wrong?
# returns the list of affected files.
.find_and_replace_unsafe = function(regex, replacement, pkg = ".") {
  pkg = devtools::as.package(pkg)
  rDirectories = c("data-raw", "R", "vignettes")
  rDirectories = rDirectories[fs::dir_exists(rDirectories)]
  # locate source files in the directories
  files = dplyr::bind_rows(lapply(rDirectories, fs::dir_info)) %>%
    dplyr::filter(fs::path_ext(path) %in% c("R", "Rmd")) %>%
    # load all the content as a list column, make a copy
    dplyr::mutate(content.old = purrr::map(path, ~ readr::read_lines(.x))) %>%
    dplyr::mutate(content = content.old)

  file = files %>%
    dplyr::mutate(
      content = purrr::map(
        content,
        ~ stringr::str_replace_all(.x, regex, replacement)
      ),
      changed = purrr::map2_lgl(content.old, content, ~ any(.x != .y))
    )

  files %>%
    dplyr::filter(changed) %>%
    purrr::pmap_chr(function(path, content, ...) {
      .write_unsafe(content, path)
    }) %>%
    unname()
}


find_and_replace = function(regex, replacement, pkg = ".") {
  pkg = devtools::as.package(pkg)
  .commit_if_needed(pkg$path, "pre find and replace")
  .find_and_replace_unsafe(regex, replacement, pkg)
}
