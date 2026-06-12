test_that("fix_global_variables function exists", {
  expect_true(is.function(pkgtools::fix_global_variables))
})

test_that("fix_global_variables has expected parameters", {
  args <- formals(pkgtools::fix_global_variables)
  expect_true("pkg" %in% names(args))
  expect_true("check" %in% names(args))
})
