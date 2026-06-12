test_that("standalone_snapshot produces style_text formatted output", {
  obj <- 42
  result <- standalone_snapshot(obj, .clip = FALSE)

  # Result should be properly formatted (no excessive whitespace)
  expect_true(is.character(result))
  expect_true(length(result) > 0)

  # Should be a valid R expression when combined
  expect_silent(parse(text = paste(result, collapse = "\n")))
})

test_that("standalone_snapshot works with NULL", {
  result <- standalone_snapshot(NULL, .clip = FALSE, .as = "null_obj")
  expect_true(is.character(result))
  expect_true(any(grepl("null_obj", result)))
})

test_that("standalone_snapshot works with empty vector", {
  result <- standalone_snapshot(integer(0), .clip = FALSE, .as = "empty")
  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
  expect_true(any(grepl("empty", result)))
})

test_that("standalone_snapshot works with factors", {
  obj <- factor(c("a", "b", "c"))
  result <- standalone_snapshot(obj, .clip = FALSE, .as = "fct")
  expect_true(is.character(result))
  expect_true(any(grepl("expect_equal", result)))
  expect_true(any(grepl("fct", result)))
})
