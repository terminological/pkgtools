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
      gert::git_add(files = ".")
      gert::git_commit(message)
    }
  }
}


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
  stop("local repository not found for: ", remote)
}

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

.find_remote_repo = function(path = getwd()) {
  root = gert::git_find(path)
  repo = gert::git_remote_info(repo = root)
  tmp = .parse_remote_url(repo$url)
  return(c(tmp, local = root))
}
