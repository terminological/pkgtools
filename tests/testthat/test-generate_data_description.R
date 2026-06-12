test_that("generate_data_description works with data frames", {
  result <- generate_data_description(
    datasets::iris,
    title = "Test iris",
    description = "A test dataset",
    name = "testiris"
  )
  expect_true(is.character(result))
  expect_true(any(grepl("testiris", result, ignore.case = TRUE)))
  expect_true(any(grepl("Test iris", result)))
  expect_true(any(grepl("dataframe with 150 rows and 5 columns", result)))
})

test_that("generate_data_description works with named lists", {
  test_list <- list(
    x = 1:5,
    y = c("a", "b", "c"),
    z = data.frame(a = 1:3, b = 4:6)
  )
  result <- generate_data_description(
    test_list,
    title = "Test list",
    description = "A named list",
    name = "testlist"
  )
  expect_true(is.character(result))
  expect_true(any(grepl("named list", result)))
  expect_true(any(grepl("testlist", result, ignore.case = TRUE)))
})

test_that("generate_data_description works with scalar values", {
  result <- generate_data_description(
    42,
    title = "A number",
    description = "Just a number",
    name = "mynumber"
  )
  expect_true(is.character(result))
  expect_true(any(grepl("mynumber", result, ignore.case = TRUE)))
})

test_that("generate_data_description defaults name to expression", {
  my_data <- datasets::mtcars
  result <- generate_data_description(
    my_data,
    title = "Test mtcars",
    description = "A test"
  )
  expect_true(any(grepl("mtcars", result, ignore.case = TRUE)))
})

test_that("generate_data_description handles long descriptions with wrapping", {
  long_text <- paste(rep("word ", 50), collapse = "")
  result <- generate_data_description(
    datasets::iris,
    description = long_text,
    name = "wrapped"
  )
  expect_true(all(nchar(result[result != ""]) <= 80))
})
