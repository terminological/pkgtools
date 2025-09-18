# from rex:::escape.character
# escape a regex
.escape = function(x) {
  chars <- c(
    "*",
    ".",
    "?",
    "^",
    "+",
    "$",
    "|",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "\\"
  )
  .sanitize(x, chars)
}

.sanitize = function(x, chars) {
  gsub(
    paste0("([\\", paste0(collapse = "\\", chars), "])"),
    "\\\\\\1",
    x,
    perl = TRUE
  )
}

.pkgtools_message = function(...) {
  if (getOption("pkgtools.verbose", FALSE)) message(...)
}


# move a file ensuring a backup exists. if the backup target exists
# then move that to a backup backup.
.move_safe = function(file, new_file = paste0(file, ".old")) {
  if (fs::file_exists(new_file)) {
    .move_safe(file = new_file)
  }
  suppressMessages(fs::file_move(file, new_file))
  return(new_file)
}

# write a file ensuring a backup exists
.write_safe = function(x, file, commit = "writing '%s'") {
  fs::dir_create(fs::path_dir(file))
  .commit_if_needed(file, sprintf(commit, file))

  if (inherits(x, "description")) {
    x$write(file = file)
  } else {
    if (length(x) > 1) {
      x = paste0(x, collapse = "\n")
    }
    suppressMessages(readr::write_file(x, file))
  }

  return(unname(file))
}

# write a file ensuring a backup exists
.write_unsafe = function(x, file) {
  if (inherits(x, "description")) {
    x$write(file = file)
  } else {
    if (length(x) > 1) {
      x = paste0(x, collapse = "\n")
    }
    suppressMessages(readr::write_file(x, file))
  }

  return(unname(file))
}

.write_merge = function(x, file) {
  if (fs::file_exists(file)) {
    old = readr::read_file(file)
    x = merge_code(old, x, lhs = "Existing file", rhs = "Update")
  }
  readr::write_lines(x)
}

# vectorises isS3stdGeneric
# .isGeneric = function(listOfFunctions) {
#   unname(sapply(listOfFunctions, function(x) tryCatch(suppressWarnings(utils::isS3stdGeneric(x)), error = function(e) FALSE)))
# }

# escapes only utf8 characters in file
.escape_utf8 = function(x) {
  x %>%
    stringi::stri_escape_unicode() %>%
    stringr::str_replace_all("\\\\u([0-9A-F]{4})", "@UTF\\1UTF@") %>%
    stringi::stri_unescape_unicode() %>%
    stringr::str_replace_all("@UTF([0-9A-F]{4})UTF@", "\\\\u\\1")
}


# if no files then change is arbitrarily far in the past.
.latestchange = function(dir, exc = "[.](Rproj|Rhistory|git.*)") {
  max(c(
    as.POSIXct("1900-01-01"),
    fs::file_info(fs::dir_ls(
      dir,
      recurse = TRUE,
      type = "file",
      regexp = exc,
      invert = TRUE
    ))$modification_time
  ))
}

# Well do yah?
.punkmode = function() {
  getOption("pkgtools.punk_mode", FALSE)
}

# All the imports of a package
.imports = function(packagename) {
  tryCatch(
    {
      pkgload::parse_deps(
        devtools::as.package(pkgload::inst(packagename))$imports
      )$name
    },
    error = function(e) character()
  )
}
