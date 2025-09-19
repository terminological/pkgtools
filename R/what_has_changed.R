#' Compare content of editor with last saved version
#'
#' I frequently end up getting an unsaved change in RStudio conflicting with
#' an update on disk. Often as a result of a global find and replace. This helps
#' determine which version to keep, by bringing up a 2 way diff of editor content
#' and disk content. The result of the merge is placed back in the editor.
#'
#' @concept edit
#' @return nothing
#' @export
what_has_changed = function() {
  context = rstudioapi::getSourceEditorContext()
  editor_content = context$content
  path = context$path
  saved_version = readr::read_lines(path)
  if (!identical(saved_version, editor_content)) {
    merged = merge_code(
      editor_content,
      saved_version,
      lhs = "editor version",
      rhs = "disk version",
      accept = "Update editor",
      rhs_accept = "Revert to saved"
    )

    rstudioapi::setDocumentContents(
      text = paste0(unlist(merged), collapse = "\n"),
      context$id
    )
    # print(diffobj::diffChr(
    #   saved_version,
    #   editor_content,
    #   interactive = FALSE,
    #   mode = "sidebyside"
    # ))
  } else {
    message("no unsaved changes")
  }
}
