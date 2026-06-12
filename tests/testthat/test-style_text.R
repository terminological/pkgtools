test_that("style_text formats code consistently", {
  code <- "x <- 1"
  result <- style_text(code)
  expect_true(is.character(result))
  expect_equal(length(result), 1)
})

test_that("style_text handles multi-line code", {
  code <- "x <- 1\ny <- 2"
  result <- style_text(code)
  expect_true(is.character(result))
  expect_true(length(result) >= 1)
})

test_that("style_text handles long lines with width parameter", {
  long_line <- "x <- function(a, b, c, d, e, f, g, h, i, j) { a + b + c + d + e + f + g + h + i + j }"
  result <- style_text(long_line, width = 50)
  expect_true(is.character(result))
  # Should break long lines into multiple lines
  expect_true(length(result) > 1)
})

test_that("style_text formats short lines consistently", {
  code <- "x <- 1"
  result <- style_text(code)
  expect_true(is.character(result))
  # pkgtools style converts <- to =
  expect_true(any(grepl("x.*=.*1", result)))
})

test_that("style_text preserves comments", {
  code <- "# a comment\nx <- 1"
  result <- style_text(code)
  expect_true(any(grepl("comment", result)))
})

test_that("style_text handles package-qualified calls", {
  code <- "dplyr::mutate(iris, x = Sepal.Length + Sepal.Width)"
  result <- style_text(code)
  expect_true(any(grepl("dplyr::mutate", result)))
})

test_that("style_text handles functions with many arguments", {
  code <- "my_function(a = 1, b = 2, c = 3, d = 4, e = 5)"
  result <- style_text(code)
  expect_true(is.character(result))
})

test_that("style_text does not break when called in non-interactive context", {
  code <- "x <- 1:10"
  result <- style_text(code)
  expect_true(is.character(result))
  expect_true(any(nchar(result) > 0))
})

test_that("style_text produces valid R code", {
  code <- "x <- 1\ny <- 2"
  result <- style_text(code)
  expect_silent(parse(text = result))
})
