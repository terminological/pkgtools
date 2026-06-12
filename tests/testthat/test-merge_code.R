test_that("merge_code returns original content when inputs are identical", {
  content <- c("line one", "line two", "line three")
  result <- merge_code(content, content)

  expect_equal(result, content)
})

test_that("merge_code handles null old content", {
  new_content <- c("new line one", "new line two")
  result <- merge_code(new_content, NULL, old = NULL)

  expect_equal(result, new_content)
})

test_that("merge_code handles null new content", {
  old_content <- c("old line one", "old line two")
  result <- merge_code(old_content, NULL, old = old_content)

  expect_equal(result, old_content)
})

test_that("merge_code with no difference returns character vector", {
  content <- c("x <- 1", "y <- 2")
  result <- merge_code(content, content, value = content)

  expect_true(is.character(result))
  expect_equal(length(result), 2)
})
