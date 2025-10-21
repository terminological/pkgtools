#' Use '@unit' unit tests in your package
#'
#' This functions amends DESCRIPTION to set up pkgtools usage.
#' Call it once when you're setting a new package.
#'
#' @param path character path to the package
#' @concept usethis
#'
#' @return Nothing: called for file system side effects.
#' @export
#'
use_unit_test = function(path = ".") {
  rlang::check_installed("desc")

  cli::cli_alert_info("Registering pkgtools usage in DESCRIPTION")

  current_roxy <- desc::desc_get("Roxygen", file = path)[[1]]
  if (is.na(current_roxy)) {
    desc::desc_set(
      "Roxygen",
      'list(markdown = TRUE, packages="pkgtools")'
    )
  } else {
    current <- eval(parse(text = current_roxy))
    new <- current
    new[["packages"]] <- union(
      current[["packages"]],
      "pkgtools"
    )

    new_string <- paste(deparse(new), collapse = "")
    desc::desc_set("Roxygen", new_string, file = path)
  }

  cli::cli_alert_info(
    "Registering pktools build-time dependency in DESCRIPTION"
  )
  desc::desc_set(
    "Config/Needs/build",
    .paste_desc(
      desc::desc_get("Config/Needs/build", file = path),
      "terminological/pkgtools"
    ),
    file = path
  )
}

#' Append value if needed
#'
#' @param x Existing DESCRIPTION field value
#' @param y Value to append
#' @noRd
#'
#' @return A character string
.paste_desc <- function(x, y) {
  if (is.na(x)) {
    return(y)
  }

  # ensure this is idempotent
  toString(unique(c(x, y)))
}
