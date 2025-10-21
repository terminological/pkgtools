#' Parser for `@unit` tags
#'
#' The `@unit` tag can be used in `roxygen2` documentation
#' of a function to define unit testing code. This code will be written to a
#' testthat file.
#'
#' @inheritParams roxygen2::roxy_tag_parse
#' @concept roxy
#'
#' @importFrom roxygen2 roxy_tag_parse
#' @return a `roxy_tag` object with the `val` field set to the parsed value
#' @export
#' @examples
#' # This provides support to `roxygen2` and only gets executed in the context
#' # of `devtools::document()`. There is no interactive use of this function.
roxy_tag_parse.roxy_tag_unit = function(x) {
  roxygen2::tag_examples(x)
}

#' Support for `@unit` tags
#'
#' The `@unit` tag can be used in `roxygen2` documentation
#' of a function to define unit testing code. This code will be written to a
#' testthat file.
#'
#' @inheritParams roxygen2::roxy_tag_rd
#'
#' @importFrom roxygen2 roxy_tag_rd
#' @concept roxy
#' @return an `roxygen2::rd_section` (see `roxygen2` documentation)
#' @export
#'
#' @examples
#' # An example function definition:
#' fn_definition <- "
#' #' This is a title
#' #'
#' #' This is the description.
#' #'
#' #' @@md
#' #' @@unit
#' #' # unit testing code here:
#' #' testthat::expect_equal(f(), \"test\")
#' #' @@export
#' f <- function() {
#'   return(\"test\")
#' }
#' "
#'
#' # For this example we manually parse the function specification in `fn_definition`
#' # creating a .Rd block - normally this is done by `roxygen2` which then
#' # writes this to an .Rd file. This function is not intended to be used
#' # outside of a call to `devtools::document`.
#'
#' fn_definition = gsub("@@","@",fn_definition)
#' tmp = roxygen2::parse_text(fn_definition)
#' print(tmp[[1]])
#' @unit
#' # for testing
#' warning("should be suppressed")
roxy_tag_rd.roxy_tag_unit = function(x, base_path, env) {
  name = NULL
  suppressMessages(suppressWarnings(force(x)))
  code = raw_code = x$raw

  # withr::with_output_sink(nullfile(), {
  block = .search_call_stack(.class = "roxy_block")
  #   block = force(block$object)
  # })

  topic = block$object$topic
  is_fn = TRUE
  if (is.null(topic)) {
    topic = try(
      purrr::keep(block$tags, ~ .x$tag == "name")[[1]]$val,
      silent = TRUE
    )
    is_fn = FALSE
  }
  if (inherits(topic, "try-error")) {
    topic = try(deparse(block$call[[2]]), silent = TRUE)
    is_fn = FALSE
  }
  if (inherits(topic, "try-error")) {
    topic = try(deparse(block$call[[1]]), silent = TRUE)
    is_fn = FALSE
  }
  if (inherits(topic, "try-error")) {
    roxygen2::warn_roxy_tag(
      x$tag,
      "Could not determine topic for unit tests use an `@name` tag to set."
    )
    return(NULL)
  }

  test_path = gsub("/R/", "/tests/testthat/test-unit-", block$file)
  fs::dir_create(fs::path_dir(test_path))
  pkg = unname(getNamespaceName(env))
  relpath = stringr::str_extract(block$file, "^(.*?/)(R/.*)$", 2)

  start_glue = "# unit test start: {name} ----"
  end_glue = "# unit test end: {name} ----"
  insert_before = "# end of unit tests ----"

  # Check to see if the test file exists. If not create and empty skeleton:
  if (!fs::file_exists(test_path)) {
    readr::write_lines(
      .create_skeleton(
        unit_test_skeleton,
        pkg = pkg,
        .insert_before = insert_before
      ),
      test_path
    )
  }

  file_lines = readr::read_lines(block$file)
  at_names = .glue_recover_pieces("#' @name {name}", file_lines)

  test_lines = readr::read_lines(test_path)

  # Get rid of tests for functions that have been removed:
  # check for blocks that match the fence format
  test_block_lines = test_lines[.glue_detect_output(start_glue, test_lines)]
  # extract names of functions under test from the fence
  test_block_data = .glue_recover_pieces(start_glue, test_block_lines)

  unit_test_names = setdiff(test_block_data$name, at_names$name)

  # delete unmatched tests
  for (name in unit_test_names) {
    # make sure function still exists in the namespace being documented
    tmp = try(eval(parse(text = name), env), silent = TRUE)
    if (inherits(tmp, "try-error") || !is.function(tmp)) {
      # TODO: This condition allows for unit testing of functions inside
      # nested environments but these will not get documentation

      test_lines = .delete_fenced_block(
        test_lines,
        name = name,
        .start_glue = start_glue,
        .end_glue = end_glue
      )
    }
  }

  # If no tests create an expect no failure, with warnings muffled.
  if (!stringr::str_detect(raw_code, "expect_.*?\\(")) {
    raw_code = whisker::whisker.render(
      no_test_code_wrapper,
      data = list(code = raw_code)
    )
  }

  # browser()
  raw_code = paste0("  ", as.character(style_text(raw_code)), collapse = "\n")

  # modify the existing block or add a new one at the end
  test_lines = .update_fenced_block(
    test_lines,
    .template = unit_test_call,
    # whisker data:
    name = topic,
    is_fn = is_fn,
    path = relpath,
    fullpath = block$file,
    pkg = pkg,
    line = x$line,

    code = raw_code,
    # fences:
    .start_glue = start_glue,
    .end_glue = end_glue,
    .insert_before = insert_before
  )

  # write the new block out to the test file
  readr::write_lines(test_lines, test_path)

  # write the code into the Rd as a "Unit tests" section
  roxygen2::rd_section("unit", list(code = format(code)))
}

#' @export
format.rd_section_unit = function(x, ...) {
  whisker::whisker.render(unit_test_section, x$value)
}

# Unit testing whisker templates ----

unit_test_section = "
{{#code}}
\\section{Unit tests}{
\\if{html}{\\out{<div class=\"sourceCode\">}}\\preformatted{
{{{.}}}
}\\if{html}{\\out{</div>}}
}
{{/code}}
"

unit_test_skeleton = "# Standalone file: do not edit by hand
# Generated by: pkgtools from @unit tags
# ----------------------------------------------------------------------
library({{{pkg}}})
"

# unit_test_call = "
# test_that(\"{{{name}}} unit test\", {
#
#   # generated by roxygen @unit tag
#   # edit this at:
#   # rstudioapi::documentOpen(path=\"{{{path}}}\", line={{{line}}})
# {{#is_fn}}
#   # or navigate to topic with <F2>
#   F2 = {{{name}}}
# {{/is_fn}}
#
# {{{code}}}
#
# testthat::expect(rlang::caller_env(n = 2)$ok,
#   failure_message = \"From the unit test at:\",
#   srcref = srcref(srcfile(system.file(\"{{{path}}}\", package=\"{{{pkg}}}\")), c({{{line}}}, {{{line}}}, 0, 0))
# )
# })
# "

unit_test_call = "
test_that(\"{{{name}}} unit test\", {

  # Automatically generated test case from roxygen @unit tag
  # Do not edit here - follow the link to the source file.
  
{{{code}}}

  # generates a failure if the overall test is failing with a link to the 
  # source of the unit test:
  testthat::expect(rlang::caller_env(n = 2)$ok,
    failure_message = \"Source link for failing @unit test.\",
    srcref = srcref(srcfile(\"../../{{{path}}}\"), c({{{line}}}, 1, {{{line}}}+1, 1))
  )
})
"

no_test_code_wrapper = "
testthat::expect_no_error(withCallingHandlers({

{{{code}}}
  
}, warning = function(e) {
  message(\"Warning issued: \",e$message)
  invokeRestart(\"muffleWarning\")
}))"
