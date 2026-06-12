#' Commit pending changes if the working tree is dirty
#'
#' @noRd
.commit_if_needed = function(path, message) {
  path = try(gert::git_find(path), silent = TRUE)
  if (inherits(path, "try-error")) {
    stop(
      "`pkgtools` assumes it is working on a Git repository for backups, but this is not the case here."
    )
  } else {
    rows = nrow(gert::git_status(repo = path))
    if (rows > 0) {
      .pkgtools_message("creating commit before changing files.")
      gert::git_add(files = ".", repo = path)
      gert::git_commit(message, repo = path)
    }
  }
  .commits$push(gert::git_commit_info(repo = path)$id)
  message(message, ": ", gert::git_commit_info(repo = path)$id)
}

#' Find a local git repository by its remote specification
#'
#' @noRd
.find_local_repo = function(remote, git_dir = fs::path_home("Git")) {
  dirs = fs::dir_ls(git_dir, type = "directory")
  for (dir in dirs) {
    try(
      {
        tmp = .find_remote_repo(dir)
        if (tmp$remote == remote) return(tmp)
      },
      silent = TRUE
    )
  }
  stop("local repository not found for: ", remote, " in ", git_dir)
}

#' Parse a git remote URL into organisation, repository, and host components
#'
#' @noRd
.parse_remote_url = function(url) {
  tmp = stringr::str_match_all(
    url,
    "^(git@|https://|http://)([^:/]+)(:|/)([^/]+)/([^\\.]+)\\.git$"
  )[[1]]
  org = tmp[, 5]
  repo_name = tmp[, 6]
  host = tmp[, 3]
  return(list(
    remote = sprintf("%s/%s", org, repo_name),
    repository = repo_name,
    organisation = org,
    host = host,
    url = url
  ))
}

#' Find the remote repo details for a path under Git version control
#'
#' @noRd
.find_remote_repo = function(path = getwd()) {
  root = gert::git_find(path)
  repo = gert::git_remote_info(repo = root)
  tmp = .parse_remote_url(repo$url)
  return(c(tmp, local = root))
}

.commits = fastmap::faststack()

#' Undo a bulk `pkgtools` operation
#'
#' Reverts to last state committed by `pkgtools`, and stashes any unstaged changes.
#' All `pkgtools` functions that modify files commit before making changes and
#' record the commit hash. This function rolls back to that commit.
#'
#' @concept usethis
#'
#' @param pkg the package
#'
#' @returns nothing - called for side effects
#' @export
#' @examples
#' if (FALSE) {
#'   # After making changes with fix_unqualified_fns_bulk():
#'   undo()
#'   # To redo the changes: gert::git_stash_pop()
#' }
undo = function(pkg = ".") {
  pkg = devtools::as.package(pkg)

  if (.commits$size() == 0) {
    message("nothing to undo.")
    return()
  }

  id = .commits$peek()
  info = gert::git_commit_info(repo = pkg$path)

  if (id != info$id) {
    stop(
      "The current HEAD does not match the last `pkgtools` undo history.
We have aborted the undo in case there are important changes committed since the last operation.
You will have to fix this manually."
    )
  }

  if (
    utils::askYesNo(
      msg = paste0(
        c("Roll back to: ", info$id, ": ", info$message),
        collapse = ""
      ),
      default = TRUE
    )
  ) {
    gert::git_stash_save(include_untracked = TRUE)
    gert::git_reset_hard(id, repo = pkg$path)
  }

  message(
    "rolled back to state prior to last pkgtools operation.\n
To redo operation use `gert::git_stash_pop()` or 
"
  )

  # git reset --hard 31e42aa760d52a03dab243db346716d137cf8882 && git clean -df
}
