test_that("fix_non_standard_files function exists", {
  expect_true(is.function(pkgtools::fix_non_standard_files))
})

test_that("fix_non_standard_files has expected parameters", {
  args <- formals(pkgtools::fix_non_standard_files)
  expect_true("pkg" %in% names(args))
  expect_true("check" %in% names(args))
})
