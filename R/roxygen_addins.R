# Run commented code ----

#' Execute commented out code
#'
#' RStudio does this for examples but nothing else this allows singly commented
#' code or code in other tags e.g. `@unit` to be executed
#'
#' This is installed as an RStudio add in and can be given a keyboard shortcut
#' through `Tools` > `Modify Keyboard Shortcuts...`
#'
#' @concept edit
#' @returns nothing
#' @export
run_commented_code = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(context$id)
  selection = gsub("(^|\\n)\\s*#'?", "\\1", selection)
  tmp = try(parse(text = selection), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    rstudioapi::showDialog("Invalid selection", format(tmp))
  } else {
    rstudioapi::sendToConsole(selection, focus = FALSE)
  }
  invisible()
}

# Insert test cases ----

## Hash based ----

#' Switch global variable for hash based test case.
#'
#' @returns nothing
#' @export
switch_standalone_snapshot = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(id = context$id)
  var = try(get(selection$value, envir = globalenv()), silent = TRUE)
  if (inherits(var, "try-error")) {
    rstudioapi::showDialog("Undefined global", selection$value)
  } else {
    text = standalone_snapshot(var, FALSE, .as = selection$value)
    rstudioapi::selectionSet(value = text, id = context$id)
  }
}

#' Generate a hash based expectation for a standalone object
#'
#' `testthat` does not use hash based checking anymore, in favour of snapshots.
#' These don't work well in tests of standalones because the file will move
#' projects independently of the test directory. This function lets you quickly
#' create a check based on a gold standard
#'
#' @param obj the gold standard to test
#' @param .clip copy the result to the clipboard (needs `clipr`)
#'
#' @returns a code snippet that can be pasted into the unit test case
#' @export
standalone_snapshot = function(
  obj,
  .clip = interactive(),
  .as = deparse(substitute(obj))
) {
  out = sprintf(
    "testthat::expect_equal(rlang::hash(%s), \"%s\")",
    .as,
    rlang::hash(obj)
  )
  if (.clip) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      message("clipboard updated: ", out)
      clipr::write_clip(out)
      return(invisible(out))
    }
  }
  return(out)
}


## Evaluation ----

#' Switch expression for equality based test case.
#'
#' @returns nothing
#' @export
switch_expect_equals = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(id = context$id)
  rstudioapi::primary_selection(context)
  rng = rstudioapi::primary_selection(context)$range

  if (rng$start[2] > 1) {
    lead = substr(
      context$contents[rng$start[1]],
      start = 1,
      stop = rng$start[2] - 1
    )
  } else {
    lead = ""
  }

  var = try(eval(parse(text = selection$value), envir = globalenv()))
  if (inherits(var, "try-error")) {
    rstudioapi::showDialog("Not able to evaluate expression", selection$value)
    stop("Failed to evaluate test expression.")
  } else {
    var_code = paste0(deparse(var), collapse = "")

    code = sprintf("testthat::expect_equal(%s, %s)", selection, var_code)
    code = style_text(code)
    code = paste0(lead, code)

    if (length(code) > 10) {
      ok = rstudioapi::showQuestion(
        "Long result",
        "The selection you are evaluating results in a lot of code. Are you sure?"
      )
      if (!ok) stop("Cancelled test creation.")
    }

    new_doc = c(
      context$contents[seq_len(max(c(rng$start[1] - 1, 0)))], # start is inclusive
      code,
      context$contents[-seq_len(rng$end[1])]
    )

    rstudioapi::setDocumentContents(
      paste0(new_doc, collapse = "\n"),
      context$id
    )
  }
}

#' testthat::expect_equal(iris[1, ], structure(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2, Species = structure(1L, levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1L, class = "data.frame"))

#' Generate a hash based expectation for a standalone object
#'
#' `testthat` does not use hash based checking anymore, in favour of snapshots.
#' These don't work well in tests of standalones because the file will move
#' projects independently of the test directory. This function lets you quickly
#' create a check based on a gold standard
#'
#' @param obj the gold standard to test
#' @param .clip copy the result to the clipboard (needs `clipr`)
#'
#' @returns a code snippet that can be pasted into the unit test case
#' @export
standalone_snapshot = function(
  obj,
  .clip = interactive(),
  .as = deparse(substitute(obj))
) {
  out = sprintf(
    "testthat::expect_equal(rlang::hash(%s), \"%s\")",
    .as,
    rlang::hash(obj)
  )
  if (.clip) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      message("clipboard updated: ", out)
      clipr::write_clip(out)
      return(invisible(out))
    }
  }
  return(out)
}
