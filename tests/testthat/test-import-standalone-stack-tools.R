test_that(".df_to_list_of_lists converts flat data frame", {
  result <- .df_to_list_of_lists(iris[1, ])

  expect_true(is.list(result))
  expect_equal(result$Species, iris$Species[1])
})

test_that(".df_to_list_of_lists handles nested data frames", {
  mtcars_nest <- datasets::mtcars %>%
    dplyr::mutate(name = rownames(datasets::mtcars)) %>%
    tidyr::nest(details = -c(cyl, gear))

  result <- .df_to_list_of_lists(mtcars_nest)

  expect_true(is.list(result))
  expect_true(is.list(result[[1]]$details))
})

test_that(".list_of_lists_to_df converts back to data frame", {
  iris_list <- .df_to_list_of_lists(iris[1, ])
  result <- .list_of_lists_to_df(iris_list)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
})

test_that(".list_of_lists_to_df and .df_to_list_of_lists are inverses", {
  input <- iris
  list_result <- .df_to_list_of_lists(input)
  df_result <- .list_of_lists_to_df(list_result)

  expect_equal(nrow(df_result), nrow(input))
  expect_equal(ncol(df_result), ncol(input))
})

test_that(".get_fn_name identifies function names", {
  f <- tolower
  expect_equal(.get_fn_name(f), "tolower")
})

test_that(".get_fn_name returns name of anonymous function in env", {
  anon <- function(x) x
  # Anonymous functions get their env-assigned name
  result <- .get_fn_name(anon)
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})

test_that(".get_fn_name returns .func_name for the function itself", {
  expect_equal(.get_fn_name(.get_fn_name), ".get_fn_name")
})

test_that(".lax_as_function passes through functions", {
  expect_true(is.function(.lax_as_function(tolower)))
  expect_equal(.lax_as_function(tolower), tolower)
})

test_that(".lax_as_function converts formula to function", {
  f <- .lax_as_function(~ tolower(.x))
  expect_true(is.function(f))
  expect_equal(f("HELLO"), "hello")
})

test_that(".lax_as_function returns primitives as-is", {
  result <- .lax_as_function("hello world")
  expect_equal(result, "hello world")
})

test_that(".search_call_stack finds first argument", {
  f <- function(x) {
    g(x)
  }
  g <- function(x) {
    h(x)
  }
  h <- function(x) {
    df <- .search_call_stack(.class = "data.frame")
    return(nrow(df))
  }

  result <- f(iris)
  expect_equal(result, nrow(iris))
})

test_that(".find_namespace returns package namespace", {
  env <- .find_namespace(rlang::abort)
  expect_equal(unname(getNamespaceName(env)), "rlang")
})

test_that(".find_namespace handles function arguments", {
  f <- function(x) {
    .find_namespace(x)
  }
  result <- f(rlang::abort)
  expect_equal(unname(getNamespaceName(result)), "rlang")
})
