test_that(".update_fenced_block inserts new block when markers not found", {
  input <- c(
    "before",
    "# insert before ----",
    "end"
  )

  template <- "# {{name}} {{status}}"
  result <- .update_fenced_block(
    input,
    template,
    name = "test",
    status = "updated",
    .start_glue = "#TEST {name} start ----",
    .end_glue = "#TEST {name} end ----",
    .insert_before = "# insert before ----"
  )

  # The block is inserted at the .insert_before position
  expect_true(any(grepl("#TEST test start ----", result)))
  expect_true(any(grepl("# test updated", result, fixed = TRUE)))
  expect_true(any(grepl("#TEST test end ----", result)))
})

test_that(".update_fenced_block replaces existing block", {
  input <- c(
    "before",
    "#TEST test start ----",
    "# old content",
    "#TEST test end ----",
    "after"
  )

  template <- "# {{name}} {{status}}"
  result <- .update_fenced_block(
    input,
    template,
    name = "test",
    status = "new",
    .start_glue = "#TEST {name} start ----",
    .end_glue = "#TEST {name} end ----"
  )

  # Old content is replaced by the new template output
  expect_true(any(grepl("# test new", result, fixed = TRUE)))
  expect_true(any(grepl("before", result)))
  expect_true(any(grepl("after", result)))
  # Old content should NOT be there - it was replaced
  expect_false(any(grepl("# old content", result)))
})

test_that(".update_fenced_block preserves surrounding content", {
  input <- c(
    "# header",
    "#TEST block start ----",
    "# original content",
    "#TEST block end ----",
    "# footer"
  )

  template <- "# new content here"
  result <- .update_fenced_block(
    input,
    template,
    name = "block",
    .start_glue = "#TEST {name} start ----",
    .end_glue = "#TEST {name} end ----"
  )

  expect_true(any(grepl("# header", result)))
  expect_true(any(grepl("# footer", result)))
  expect_true(any(grepl("# new content here", result, fixed = TRUE)))
})

test_that(".delete_fenced_block removes content between markers", {
  input <- c(
    "before",
    "remove this",
    "remove that",
    "after"
  )

  result <- .delete_fenced_block(
    input,
    .start_glue = input[2],
    .end_glue = input[3]
  )

  expect_true(!any(grepl("remove this", result)))
  expect_true(!any(grepl("remove that", result)))
  expect_true(any(grepl("before", result)))
  expect_true(any(grepl("after", result)))
})

test_that(".delete_fenced_block handles multiple blocks", {
  input <- rep(c("a", "b", "c"), 3)  # abc abc abc

  result <- .delete_fenced_block(
    input,
    .start_glue = "b",
    .end_glue = "c"
  )

  expect_true(all(result == "a"))
  expect_equal(length(result), 3)
})

test_that(".delete_fenced_block returns NULL when no markers found", {
  input <- c("one", "two", "three")
  result <- .delete_fenced_block(
    input,
    .start_glue = "nonexistent",
    .end_glue = "also_nonexistent"
  )

  expect_null(result)
})
