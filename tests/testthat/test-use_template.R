test_that("use_template function exists and has expected signature", {
  expect_true(is.function(pkgtools::use_template))
  args <- formals(pkgtools::use_template)
  expect_true(".template_name" %in% names(args))
  expect_true(".destfile" %in% names(args))
  expect_true(".overwrite" %in% names(args))
})
