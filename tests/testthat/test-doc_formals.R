test_that("doc_formals returns correct output for match.arg parameters", {
  test_fn <- function(
    arg = c("option one", "option two", "option three"),
    num = 123,
    ghi
  ) {
    arg <- match.arg(arg)
    return(arg)
  }

  expect_match(doc_formals(test_fn, arg), "option one")
  expect_match(doc_formals(test_fn, arg), "allowed:")
  expect_match(doc_formals(test_fn, arg), "default")
})

test_that("doc_formals returns output for atomic default values", {
  test_fn <- function(num = 123) {
    num
  }
  result <- doc_formals(test_fn, num)
  expect_match(result, "123")
  expect_match(result, "default")
})

test_that("doc_formals returns source for unevaluated defaults", {
  test_fn <- function(ghi = sum(1, 2)) {
    ghi
  }
  result <- doc_formals(test_fn, ghi)
  expect_match(result, "sum")
})

test_that("doc_formals handles missing parameters", {
  test_fn <- function(a = 1, b = 2) {
    a + b
  }
  # When parameter doesn't exist in function, doc_formals returns formatted NULL
  result <- doc_formals(test_fn, nonexistent_param)
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that("doc_formals handles single-value match.arg correctly", {
  test_fn <- function(arg = "single") {
    arg <- match.arg(arg)
    arg
  }
  result <- doc_formals(test_fn, arg)
  expect_match(result, "single")
})
