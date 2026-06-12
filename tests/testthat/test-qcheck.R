test_that("qcheck is a function", {
  expect_true(is.function(pkgtools::qcheck))
})

test_that("qcheck has expected parameters", {
  args <- formals(pkgtools::qcheck)
  expect_true("pkg" %in% names(args))
  expect_true("args" %in% names(args))
  expect_true("quiet" %in% names(args))
})
