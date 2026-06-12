test_that("bump_dev_version increments dev version", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)
  dir.create(file.path(test_dir, "R"))

  writeLines(c("Package: testpkg", "Title: Test", "Version: 1.0.0.8999"),
    file.path(test_dir, "DESCRIPTION")
  )

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  result <- bump_dev_version(test_dir)

  expect_match(result, "^1\\.0\\.0\\.")
  expect_gt(as.integer(sub(".*\\.", "", result)), 8999)

  desc <- readLines(file.path(test_dir, "DESCRIPTION"))
  expect_true(any(grepl(sprintf("Version: %s", result), desc)))

  unlink(test_dir, recursive = TRUE)
})

test_that("bump_dev_version handles non-dev versions", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)
  dir.create(file.path(test_dir, "R"))

  writeLines(c("Package: testpkg", "Title: Test", "Version: 1.0.0"),
    file.path(test_dir, "DESCRIPTION")
  )

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  result <- bump_dev_version(test_dir)

  expect_match(result, "^1\\.0\\.0\\.")

  unlink(test_dir, recursive = TRUE)
})

test_that("bump_dev_version bumps patch version", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)
  dir.create(file.path(test_dir, "R"))

  writeLines(c("Package: testpkg", "Title: Test", "Version: 0.1.0.5"),
    file.path(test_dir, "DESCRIPTION")
  )

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  result <- bump_dev_version(test_dir)

  expect_equal(result, "0.1.0.6")

  unlink(test_dir, recursive = TRUE)
})
