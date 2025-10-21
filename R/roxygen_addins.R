# Comment out code including ROxygen ----

# TODO: never use roxygen blocks
# Change roxygen block when being commented to #"

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
  # selection = rstudioapi::selectionGet(context$id)
  rng = rstudioapi::primary_selection(context)$range
  # rows = rng[["start"]][["row"]]:rng[["end"]][["row"]]
  # expand multi row selections to include while lines
  # if (length(rows) == 1 && rng[["start"]][["column"]] > 1) {
  selection = rstudioapi::selectionGet(id = context$id)
  selection = unlist(strsplit(selection$value, "\n"))
  # } else {
  # selection = context$contents[rows]
  # }

  if (length(selection) > 1 || rng[["start"]][["column"]] == 1) {
    # Selction is multiline or includes start of this line.
    test = selection
    # ignore first line for multi-line selections:
    if (length(test) > 1) {
      test = test[-1]
    }

    # Uncomment if all but the first line matches
    if (all(grepl("^\\s*#'", test))) {
      # strip off roxygen comments
      # unless they are @roxygen tags
      selection = gsub("^\\s*#'(?!\\s*@)", "", selection, perl = TRUE)
    } else if (all(grepl("^\\s*#", test))) {
      # strip off normal comments (1 level)
      selection = gsub("^\\s*#", "", selection)
    }
  }
  tmp = try(parse(text = selection), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    message("Selection does not parse: ", format(tmp))
  } else {
    rstudioapi::sendToConsole(code = selection, focus = FALSE)
  }
  invisible()
}

# Insert test cases ----

## Hash based ----

#' Switch global variable for hash based test case.
#'
#' @concept edit
#' @returns nothing
#' @export
switch_standalone_snapshot = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(id = context$id)
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

  expr = try(parse(text = selection$value))
  if (inherits(expr, "try-error")) {
    rstudioapi::showDialog("Not able to parse expression:", selection$value)
    stop("Failed to parse test expression.")
  }

  var = try(eval(expr, envir = globalenv()), silent = TRUE)

  if (inherits(var, "try-error")) {
    msg = attr(var, "condition")$message
    rstudioapi::showDialog("Error evaluating selection: ", msg)
    stop("Failed to evaluate test expression.")
  }

  code = standalone_snapshot(var, FALSE, .as = selection$value)
  code = paste0(lead, as.character(code))

  if (length(code) > 20) {
    ok = rstudioapi::showQuestion(
      "Long result",
      paste0(c(
        "The selection you are evaluating results ",
        length(code),
        " lines of code. Are you sure?"
      ))
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

  rstudioapi::setCursorPosition(rng$start, context$id)
}

#' Generate a hash based expectation for a standalone object
#'
#' `testthat` does not use hash based checking anymore, in favour of snapshots.
#' These don't work well in tests of standalones because the file will move
#' projects independently of the test directory. This function lets you quickly
#' create a check based on a gold standard
#'
#' @concept edit
#' @param obj the gold standard to test
#' @param .clip copy the result to the clipboard (needs `clipr`)
#' @param .as what name to use for the comparison
#'
#' @returns a code snippet that can be pasted into the unit test case
#' @keywords internal
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

  out = style_text(out)

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
#' @concept edit
#' @returns nothing
#' @export
switch_expect_equals = function() {
  context = rstudioapi::getSourceEditorContext()
  selection = rstudioapi::selectionGet(id = context$id)
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

  expr = try(parse(text = selection$value))
  if (inherits(expr, "try-error")) {
    rstudioapi::showDialog("Not able to parse expression:", selection$value)
    stop("Failed to parse test expression.")
  }

  var = try(eval(expr, envir = globalenv()), silent = TRUE)
  if (inherits(var, "try-error")) {
    message("Failed to evaluate test expression: testing for error.")

    message = attr(var, "condition")$message
    message = deparse(message)

    code = sprintf(
      "testthat::expect_error({%s}, %s, fixed=TRUE)",
      selection,
      message
    )
  } else {
    var_code = paste0(deparse(var), collapse = "")

    code = sprintf("testthat::expect_equal(%s, %s)", selection, var_code)
  }

  code = style_text(code)
  code = paste0(lead, as.character(code))

  if (length(code) > 20) {
    ok = rstudioapi::showQuestion(
      "Long result",
      paste0(c(
        "The selection you are evaluating results ",
        length(code),
        " lines of code. Are you sure?"
      ))
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

  rstudioapi::setCursorPosition(rng$start, context$id)
}
