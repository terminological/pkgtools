# from rex:::escape.character
# escape a regex
.escape = function (x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[",
             "]", "{", "}", "\\")
  .sanitize(x, chars)
}

.sanitize = function (x, chars) {
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"),
       "\\\\\\1", x, perl = TRUE)
}

# move a file ensuring a backup exists
.move_safe = function(file, new_file = paste0(file,".old")) {
  if (fs::file_exists(new_file)) .move_safe(file = new_file)
  suppressMessages(fs::file_move(file, new_file))
  return(new_file)
}

# write a file ensuring a backup exists
.write_safe = function(x, file, dry_run = FALSE) {
  
  dry_run = dry_run && !.punkmode()
  
  fs::dir_create(fs::path_dir(file))
  if (dry_run) {
    outfile = paste0(file,".dry_run")
  } else {
    if (!.punkmode() && fs::file_exists(file)) message("backing up original to: ", .move_safe(file))
    outfile = file
  }
  
  #
  if (inherits(x,"description")) {
    x$write(file=outfile)
  } else {
    if (length(x) > 1) x = paste0(x,collapse="\n")
    suppressMessages(readr::write_file(x,outfile))
  }
  
  return(unname(file))
  
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
    fs::file_info(fs::dir_ls(dir,recurse = TRUE,type = "file",regexp = exc,invert = TRUE))$modification_time
  ))
}

# Well do yah?
.punkmode = function() {
  getOption("pkgtools.punk_mode",FALSE)
}

.imports = function(packagename) {
  tryCatch({
    pkgload::parse_deps(devtools::as.package(pkgload::inst("tidyverse"))$imports)$name
  }, error = function(e) character())
}

