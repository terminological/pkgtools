test_that("set_renv_repos adds new repositories", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  lockfile <- file.path(test_dir, "renv.lock")
  writeLines('{
    "R": {
      "Repositories": [
        {"Name": "CRAN", "URL": "https://cloud.r-project.org"}
      ]
    }
  }', lockfile)

  pkgtools::set_renv_repos(terminological = "https://terminological.r-universe.dev", .wd = test_dir)

  lock_content <- jsonlite::read_json(lockfile)
  names <- sapply(lock_content$R$Repositories, `[[`, "Name")
  expect_true("terminological" %in% names)

  unlink(test_dir, recursive = TRUE)
})

test_that("set_renv_repos updates existing repositories", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  lockfile <- file.path(test_dir, "renv.lock")
  writeLines('{
    "R": {
      "Repositories": [
        {"Name": "CRAN", "URL": "https://cloud.r-project.org"},
        {"Name": "terminological", "URL": "https://old.example.com"}
      ]
    }
  }', lockfile)

  pkgtools::set_renv_repos(terminological = "https://new.example.com", .wd = test_dir)

  lock_content <- jsonlite::read_json(lockfile)
  terms <- Filter(function(x) x$Name == "terminological", lock_content$R$Repositories)
  expect_equal(terms[[1]]$URL, "https://new.example.com")

  unlink(test_dir, recursive = TRUE)
})

test_that("set_renv_repos adds repositories to beginning", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  lockfile <- file.path(test_dir, "renv.lock")
  writeLines('{
    "R": {
      "Repositories": [
        {"Name": "CRAN", "URL": "https://cloud.r-project.org"}
      ]
    }
  }', lockfile)

  pkgtools::set_renv_repos(
    first = "https://first.example.com",
    second = "https://second.example.com",
    .wd = test_dir
  )

  lock_content <- jsonlite::read_json(lockfile)
  names <- sapply(lock_content$R$Repositories, `[[`, "Name")

  # The first name added appears first in the reversed iteration
  expect_equal(names[1], "first")
  expect_equal(names[2], "second")

  unlink(test_dir, recursive = TRUE)
})

test_that("set_renv_repos stops on missing lockfile", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  expect_error(
    pkgtools::set_renv_repos(test = "https://example.com", .wd = test_dir),
    "No lock file found"
  )

  unlink(test_dir, recursive = TRUE)
})
