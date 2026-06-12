#' Set up a package for pkgtools `@unit` tag usage
#'
#' This function amends `DESCRIPTION` to register pkgtools as a build-time
#' dependency and adds `pkgtools` to the Roxygen `packages` field. Call it
#' once when setting up a new package that will use `@unit` and `@dparam`
#' custom roxygen tags.
#'
#' @param path character path to the package
#' @concept usethis
#'
#' @return Nothing: called for file system side effects.
#' @export
#' @examples
#' if (FALSE) {
#'   use_unit_test("/path/to/my/package")
#' }
use_unit_test = function(path = ".") {
  rlang::check_installed("desc")

  cli::cli_alert_info("Registering pkgtools usage in DESCRIPTION")

  current_roxy <- desc::desc_get("Roxygen", file = path)[[1]]
  if (is.na(current_roxy)) {
    desc::desc_set(
      "Roxygen",
      'list(markdown = TRUE, packages="pkgtools")',
      file = path
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
