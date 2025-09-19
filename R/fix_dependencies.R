#' Fixes dependencies in the namespace file using the output of R CMD check.
#'
#' @param pkg the package to scan
#' @param check output of a `devtools::check()` command
#'   (will check automatically if not provided)
#'
#' @concept usethis
#'
#' @return nothing
#' @export
fix_dependencies = function(pkg = ".", check) {
  pkg = devtools::as.package(pkg)
  if (rlang::is_missing(check)) {
    check = qcheck(pkg = pkg$path, quiet = TRUE)
  }

  desc_path = fs::path(pkg$path, "DESCRIPTION")
  desc = desc::desc(file = desc_path)
  # imports = desc$Imports %>% stringr::str_split(",\ss")

  noteswarns = paste0(c(check$warnings, check$notes), collapse = "\n")

  nsMissing = noteswarns %>%
    stringr::str_extract(
      "imports? not declared from:(([\\n\\r\\s]*(\u2018[^\u2019]+\u2019)+)*)",
      1
    ) %>%
    stringr::str_match_all("\u2018([^\u2019]+)\u2019") %>%
    purrr::map(~ .x[, 2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)

  nsUnneeded = noteswarns %>%
    stringr::str_extract(
      "not imported from:(([\\n\\r\\s]*(\u2018[^\u2019]+\u2019)+)*)",
      1
    ) %>%
    stringr::str_match_all("\u2018([^\u2019]+)\u2019") %>%
    purrr::map(~ .x[, 2]) %>%
    purrr::list_c() %>%
    purrr::discard(is.na)

  if (length(nsUnneeded) + length(nsMissing) > 0) {
    message("fixing imports/suggests issues.")
    if (length(nsUnneeded) > 0) {
      lapply(nsUnneeded, desc$del_dep)
      message(
        "removed unnecessary packages: ",
        paste0(nsUnneeded, collapse = ", ")
      )
    } else {
      message("no unnecesary packages")
    }

    if (length(nsMissing) > 0) {
      message("added missing packages: ", paste0(nsMissing, collapse = ", "))
      lapply(nsMissing, desc$set_dep, type = "Imports")
    } else {
      message("no missing packages")
    }

    .write_safe(desc, desc_path)
  } else {
    message("no imports/suggests issues.")
  }
}
