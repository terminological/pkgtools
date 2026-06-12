test_that(".glue_detect_output detects matching glue output", {
  content <- c(
    "not a match",
    "  # one, two and three ----",
    "also not a match",
    "  # X, Y and Z ----"
  )

  result <- .glue_detect_output(
    "# {name}, {value1} and {value2} ----",
    content
  )

  # Both "one, two and three" and "X, Y and Z" match the pattern
  expect_equal(result, c(FALSE, TRUE, FALSE, TRUE))
})

test_that(".glue_detect_output filters non-matching lines", {
  content <- c(
    "# a, b and c ----",
    "no markers here",
    "# x, y and z ----"
  )

  result <- .glue_detect_output("# {a}, {b} and {c} ----", content)

  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that(".glue_recover_pieces extracts glue variables", {
  glue_spec <- "# {name}, {value1} and {value2} ----"
  output <- c(
    "  # one, two and three ----",
    "  # alpha, beta and gamma ----"
  )

  result <- .glue_recover_pieces(glue_spec, output)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$name[1], "one")
  expect_equal(result$value1[1], "two")
  expect_equal(result$value2[1], "three")
})

test_that(".glue_recover_pieces regenerates original output", {
  glue_spec <- "# {name}, {value1} and {value2} ----"
  output <- c(
    "  # one, two and three ----",
    "  # alpha, beta and gamma ----"
  )

  result <- .glue_recover_pieces(glue_spec, output)

  regenerated <- glue::glue_data(result, glue_spec)
  expect_equal(trimws(output), trimws(regenerated))
})
