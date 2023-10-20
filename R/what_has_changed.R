#' Compare content of editor with last saved version
#'
#' @return nothing 
#' @export
what_has_changed = function() {
  context = rstudioapi::getSourceEditorContext()
  editor_content = context$content
  path = context$path
  saved_version = readr::read_lines(path) 
  if (!identical(saved_version, editor_content)) {
    print(diffobj::diffChr(saved_version, editor_content, interactive = FALSE, mode="sidebyside"))
  } else {
    message("no unsaved changes")
  }
}