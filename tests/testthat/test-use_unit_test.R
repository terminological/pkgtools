test_that("use_unit_test adds pkgtools to Roxygen packages", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  # Create a minimal DESCRIPTION without pkgtools
  writeLines(
    c("Package: testpkg", "Title: Test", "Version: 0.0.1",
      "Roxygen: list(markdown = TRUE)"),
    file.path(test_dir, "DESCRIPTION")
  )

  pkgtools::use_unit_test(test_dir)

  desc_content <- readLines(file.path(test_dir, "DESCRIPTION"))
  expect_true(any(grepl("pkgtools", desc_content)))
  expect_true(any(grepl("packages", desc_content, ignore.case = TRUE)))

  unlink(test_dir, recursive = TRUE)
})

test_that("use_unit_test does not duplicate pkgtools", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(
    c("Package: testpkg", "Title: Test", "Version: 0.0.1",
      'Roxygen: list(markdown = TRUE, packages="pkgtools")'),
    file.path(test_dir, "DESCRIPTION")
  )

  pkgtools::use_unit_test(test_dir)

  desc_content <- readLines(file.path(test_dir, "DESCRIPTION"))
  pkgtools_count <- sum(grepl('pkgtools"', desc_content))
  expect_equal(pkgtools_count, 1)

  unlink(test_dir, recursive = TRUE)
})

test_that("use_unit_test adds Config/Needs/build", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(
    c("Package: testpkg", "Title: Test", "Version: 0.0.1"),
    file.path(test_dir, "DESCRIPTION")
  )

  pkgtools::use_unit_test(test_dir)

  desc_content <- readLines(file.path(test_dir, "DESCRIPTION"))
  expect_true(any(grepl("Config/Needs/build", desc_content)))

  unlink(test_dir, recursive = TRUE)
})

test_that("use_unit_test preserves existing Roxygen config", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(file.path(test_dir, "R"), recursive = TRUE)

  writeLines(
    c("Package: testpkg", "Title: Test", "Version: 0.0.1",
      'Roxygen: list(markdown = TRUE, packages="otherpkg")'),
    file.path(test_dir, "DESCRIPTION")
  )

  pkgtools::use_unit_test(test_dir)

  desc_content <- readLines(file.path(test_dir, "DESCRIPTION"))
  expect_true(any(grepl("otherpkg", desc_content)))
  expect_true(any(grepl("pkgtools", desc_content)))

  unlink(test_dir, recursive = TRUE)
})
