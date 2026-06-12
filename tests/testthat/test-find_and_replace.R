test_that("find_and_replace finds and replaces in code", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(c("Package: testpkg", "Title: Test", "Version: 0.0.1"),
    file.path(test_dir, "DESCRIPTION")
  )
  file.create(file.path(test_dir, ".Rbuildignore"))

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  r_file <- file.path(test_dir, "R", "test.R")
  writeLines(c("old_func <- function(x) {", "  x + 1", "}"), r_file)

  result <- find_and_replace(
    "old_func", "new_func",
    pkg = test_dir,
    rDirectories = file.path(test_dir, "R")
  )

  expect_true(is.character(result))
  content <- readLines(r_file, warn = FALSE)
  expect_true(any(grepl("new_func", content)))
  expect_true(!any(grepl("old_func", content)))

  unlink(test_dir, recursive = TRUE)
})

test_that("find_and_replace does nothing when no match", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(c("Package: testpkg", "Title: Test", "Version: 0.0.1"),
    file.path(test_dir, "DESCRIPTION")
  )
  file.create(file.path(test_dir, ".Rbuildignore"))

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  r_file <- file.path(test_dir, "R", "test.R")
  writeLines("existing_func <- function(x) { x }", r_file)
  original_content <- readLines(r_file)

  find_and_replace(
    "nonexistent", "replacement",
    pkg = test_dir,
    rDirectories = file.path(test_dir, "R")
  )

  new_content <- readLines(r_file, warn = FALSE)
  expect_equal(new_content, original_content)

  unlink(test_dir, recursive = TRUE)
})

test_that("find_and_replace supports regex patterns", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(c("Package: testpkg", "Title: Test", "Version: 0.0.1"),
    file.path(test_dir, "DESCRIPTION")
  )

  gert::git_init(test_dir)
  gert::git_add(".", repo = test_dir)
  gert::git_commit("init", repo = test_dir)

  r_file <- file.path(test_dir, "R", "test.R")
  writeLines(c("old_a <- 1", "old_b <- 2", "keep_this <- 3"), r_file)

  find_and_replace(
    "^old_", "new_",
    pkg = test_dir,
    rDirectories = file.path(test_dir, "R")
  )

  content <- readLines(r_file, warn = FALSE)
  expect_true(any(grepl("^new_a", content)))
  expect_true(any(grepl("^new_b", content)))
  expect_true(any(grepl("^keep_this", content)))

  unlink(test_dir, recursive = TRUE)
})
