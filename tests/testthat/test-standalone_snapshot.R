test_that("standalone_snapshot creates hash-based expectations", {
  obj <- 42
  result <- standalone_snapshot(obj, .clip = FALSE, .as = "my_obj")

  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
  expect_true(any(grepl("rlang::hash", result)))
  expect_true(any(grepl("my_obj", result)))
  expect_true(any(grepl('"[0-9a-f]+"', result)))
})

test_that("standalone_snapshot works with vectors", {
  obj <- c(1, 2, 3)
  result <- standalone_snapshot(obj, .clip = FALSE, .as = "vec")

  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
  expect_true(any(grepl("vec", result)))
})

test_that("standalone_snapshot works with lists", {
  obj <- list(a = 1, b = 2)
  result <- standalone_snapshot(obj, .clip = FALSE, .as = "my_list")

  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
})

test_that("standalone_snapshot works with data frames", {
  obj <- data.frame(x = 1:3, y = letters[1:3])
  result <- standalone_snapshot(obj, .clip = FALSE, .as = "df_obj")

  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
  expect_true(any(grepl("df_obj", result)))
})

test_that("standalone_snapshot produces consistent hash", {
  obj <- "consistent value"
  result1 <- standalone_snapshot(obj, .clip = FALSE)
  result2 <- standalone_snapshot(obj, .clip = FALSE)

  expect_equal(result1, result2)
})

test_that("standalone_snapshot returns character vector", {
  obj <- "test"
  result <- standalone_snapshot(obj, .clip = FALSE)

  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true(all(nchar(result) > 0))
})

test_that("standalone_snapshot handles complex nested structures", {
  obj <- list(
    nested = list(a = 1, b = list(c = 2)),
    vector = 1:10
  )
  result <- standalone_snapshot(obj, .clip = FALSE)

  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
})

test_that("standalone_snapshot result is valid R code", {
  obj <- 42
  result <- standalone_snapshot(obj, .clip = FALSE)

  expect_silent(parse(text = result))
})
