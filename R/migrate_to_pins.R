migrate_to_pins = function(pkg = ".", board = "data", migrate = NULL) {
  pkg = devtools::as.package(pkg)
  devtools::load_all(pkg$path)

  dataenv = rlang::new_environment()
  tmp2 = data(package = pkg$package)
  items = tmp2$results[, "Item"]
  data(list = items, package = pkg$package, envir = dataenv)
  assets_dir = fs::path(pkg$path, "pkgdown/assets", board)

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

  changed = sprintf(
    "
.pin_board = .singleton(pins::board_url(\"%s\"))
",
    pages_url
  )
  for (item in items) {
    data = get(item, envir = dataenv)

    do_migrate = migrate == "*" ||
      item %in% migrate ||
      utils::askYesNo(
        sprintf("Migrate %s? (%s)", item, format(object.size(data))),
        default = TRUE
      )
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

      migrate = c(
        sprintf(
          "
#' @export
%s = function() {pins::pin_read(.pin_board(),\"%s\")}
",
          item,
          item
        )
      )

      changed = c(changed, migrate)
    }
  }

  if (!is.null(changed)) {
    .commit_if_needed(pkg$path)

    usethis::with_project(
      pkg$path,
      usethis::use_standalone("terminological/ggrrr", "standalone-singleton.R")
    )

    pins::write_board_manifest(pinboard)

    if (fs::dir_exists(fs::path(pkg$path, "docs"))) {
      fs::file_create(fs::path(pkg$path, "docs", ".nojekyll"))
      fs::dir_copy(assets_dir, fs::path(pkg$path, "docs", board))
    }

    .write_unsafe(desc, desc_path)

    .write_merge(
      paste0(changed, collapse = ""),
      fs::path(pkg$path, "R", "zz_pins.R")
    )
  }
}
