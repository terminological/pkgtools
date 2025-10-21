#' Migrate package data to a github hosted pin board
#'
#' This function makes multiple changes to the package. It adds packaged data
#' files from the `data` directory to a `pkgdown` github published pin board and
#' constructs a minimal set of functions with the same names as the data to
#' access the data from the pinboard. It finds any references to the dataset in
#' the code directories (`data-raw`,`R`,`vignettes`,`test`) and replaces them
#' with a function call of the same name.
#'
#' @concept usethis
#'
#' @param pkg the package (defaults to current)
#' @param board the board name (this is generally not exposed to user)
#' @param migrate either "*" for all data files, a list of names or if null the
#'   user will be asked interactively.
#'
#' @returns nothing
#' @export
migrate_to_pins = function(pkg = ".", board = "data", migrate = NULL) {
  pkg = devtools::as.package(pkg)
  devtools::load_all(pkg$path)

  dataenv = rlang::new_environment()
  tmp2 = data(package = pkg$package)
  items = tmp2$results[, "Item"]
  data(list = items, package = pkg$package, envir = dataenv)
  assets_dir = fs::path(pkg$path, "pkgdown/assets", board)

  .commit_if_needed(pkg$path, "migrating data to pins")

  pinboard = pins::board_folder(assets_dir)

  repo = .find_remote_repo(pkg$path)
  if (repo$host != "github.com") {
    stop("This only works for github hosted projects")
  }
  pages_url = sprintf(
    "https://%s.github.io/%s/%s/",
    repo$organisation,
    repo$repository,
    board
  )

  desc_path = fs::path(pkg$path, "DESCRIPTION")
  desc = desc::desc(file = desc_path)
  desc$set_dep("pins", type = "Imports")

  moved = NULL
  for (item in items) {
    data = get(item, envir = dataenv)

    if (is.null(migrate)) {
      do_migrate = utils::askYesNo(
        sprintf("Migrate %s? (%s)", item, format(utils::object.size(data))),
        default = TRUE
      )
    } else {
      do_migrate = migrate == "*" || item %in% migrate
    }

    if (is.na(do_migrate)) {
      stop("Migrate cancelled.")
    }

    if (do_migrate) {
      pins::pin_write(pinboard, data, name = item)
      fs::dir_create(pkg$path, "data", "old")
      fs::file_move(
        fs::path(pkg$path, "data", paste0(item, ".rda")),
        fs::path(pkg$path, "data", "old", paste0(item, ".rda"))
      )

      moved = c(moved, item)
    }
  }

  if (!is.null(moved)) {
    usethis::with_project(
      pkg$path,
      usethis::use_standalone("terminological/ggrrr", "standalone-singleton.R")
    )

    pins::write_board_manifest(pinboard)

    if (fs::dir_exists(fs::path(pkg$path, "docs"))) {
      fs::file_create(fs::path(pkg$path, "docs", ".nojekyll"))
      fs::dir_delete(fs::path(pkg$path, "docs", board))
      fs::dir_copy(
        assets_dir,
        fs::path(pkg$path, "docs", board),
        overwrite = TRUE
      )
    }

    .write_unsafe(desc, desc_path)

    # item = "test"
    # Positives:
    # positives = c("zsdfds test zdsgdf", "test sdfsd", "sdfsd test", "test == sdfsd", "test, asdas", "test; asdas", "#' test")
    # Negatives:
    # negatives = c("#' @name test", "zsdfds test() zdsgdf" , "zsdfds 'test' zdsgdf", "list$test sdfsd", "zsdfds test_me zdsgdf", "zsdfds my_test zdsgdf", "test = asdsa", "test <- asdsa")
    # gsub(sprintf(regex, item), "test()", c(positives, negatives), perl = TRUE)
    for (item in moved) {
      # unqualified instances

      # Must have space or beginning of line before
      # Must match the name
      # Must end with space or end of line or comma or semicolon
      # Mustn't end with assignment (single equals or <-) +/- space
      # can end with equality check
      # same rules for qualified version.
      # wont replace in comments
      # regex = "(?!\\#.*)(?<=\\s|^)%s(?=\\s|$|,|;)(?!\\s+=[^=])(?!\\s+<-)"
      regex = "(^\\s*#.*@.*$)(*SKIP)(*F)|(?<=\\s|^)%s(?=\\s|$|,|;)(?!\\s+=[^=])(?!\\s+<-)"
      # regex = "(?:^.*#.*$)(*SKIP)(*FAIL)|(?<=\\s|^)%s(?=\\s|$|,|;)(?!\\s+=[^=])(?!\\s+<-)"

      .find_and_replace_unsafe(
        sprintf(regex, item),
        sprintf("%s::%s()", pkg$package, item),
        rDirectories = c("R", "vignettes", "tests")
      )
      # qualified instances
      .find_and_replace_unsafe(
        sprintf(regex, sprintf("%s::%s", pkg$package, item)),
        sprintf("%s::%s()", pkg$package, item),
        rDirectories = c("R", "vignettes", "tests")
      )
    }

    contents = pins::pin_search(pinboard)$name

    changed = c(
      sprintf(
        "
# setup package internal pin board function ----
.pin_board = .singleton({
  folder = system.file(\"pkgdown/assets/%s/\", package = \"%s\")
  if (dir.exists(folder)) {
    pins::board_folder(folder)
  } else {
    pins::board_url(\"%s\")
  }
})

# package external data access functions ----
",
        board,
        pkg$package,
        pages_url
      ),
      sprintf(
        "
#' @export
%s = function() {pins::pin_read(.pin_board(),\"%s\")}
",
        contents,
        contents
      )
    )

    .write_unsafe(
      changed,
      fs::path(pkg$path, "R", "zz_pins.R")
    )
  }
}
