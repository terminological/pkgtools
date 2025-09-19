# TODO:
# 1) needs refactoring to allow this to work in parallel on all the functions in
# all packages (i.e. integrates function discovery)
# 2) create a stack and read parsed code sequentially looking for scope and assignment
# operations.
# 3) identify symbols that are references to functions (i.e. they match functions
# and there is no symbol assignment in scope)

# getParseData to handle both function calls and function references
# content = readr::read_lines(here::here("tests/testthat/test_functions.R"))
# functionNames <- c("filter", "mutate")
# package_name =  "dplyr"
# .qualify_with_parse_data(content, c("filter", "mutate"), "dplyr")
# .qualify_with_parse_data(content, c("ggplot", "geom_point", "aes"), "ggplot2")
# .qualify_with_parse_data(content, c("rnorm","quantile"), "stats")
.qualify_with_parse_data <- function(
  content,
  functionNames,
  package_name,
  references = FALSE
) {
  # Join content into a single string for parsing
  code_text <- paste(content, collapse = "\n")

  # Parse with source information
  tryCatch(
    {
      parsed <- parse(text = code_text, keep.source = TRUE)
      parse_data <- utils::getParseData(parsed)
    },
    error = function(e) {
      stop("Failed to parse R code: ", e$message)
    }
  )

  # Filter for SYMBOL tokens that are function names
  tokens = if (references) {
    c("SYMBOL", "SYMBOL_FUNCTION_CALL")
  } else {
    c("SYMBOL_FUNCTION_CALL")
  }

  # TODO: work out some heuristics for determining scope of variables using parse_data
  # and exclude SYMBOL tokens that have a SYMBOL or SYMBOL_FORMALS assigned in the
  # same function. This is the logic though that check uses
  # locals = parse_data %>%
  #   dplyr::filter(terminal != FALSE) %>%
  #   dplyr::filter(
  #     token == "SYMBOL_FORMALS" |
  #       (token == "SYMBOL" &
  #         dplyr::lead(token, default = "NO") %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  #   )
  # browser()

  unqualified_symbols = parse_data %>%
    dplyr::filter(
      token %in%
        tokens & # a function call
        text %in% functionNames & # one of the current functions
        !(
          # already qualified
          dplyr::lag(token, default = "NO") == "NS_GET" |
            # R6 / list method call.
            dplyr::lag(token, default = "NO") == "'$'"
        )
    ) %>%
    dplyr::mutate(
      symbol_type <- ifelse(
        token == "SYMBOL_FUNCTION_CALL",
        "call",
        "reference"
      )
    ) %>%
    dplyr::arrange(
      # Process modifications from right to left, bottom to top
      dplyr::desc(line1),
      dplyr::desc(col1)
    )

  # Apply modifications
  modified_content <- content

  for (i in seq_len(nrow(unqualified_symbols))) {
    func_name <- unqualified_symbols$text[[i]]
    line_num <- unqualified_symbols$line1[[i]]
    col_start <- unqualified_symbols$col1[[i]]
    col_end <- unqualified_symbols$col2[[i]]

    # Modify the line
    if (line_num <= length(modified_content)) {
      line <- modified_content[line_num]
      before <- substr(line, 1, col_start - 1)
      after <- substr(line, col_end + 1, nchar(line))
      qualified_name <- paste0(package_name, "::", func_name)
      modified_content[line_num] <- paste0(before, qualified_name, after)
    }
  }

  summary = unqualified_symbols %>%
    dplyr::group_by(name = text) %>%
    dplyr::summarise(value = dplyr::n()) %>%
    dplyr::mutate(pkg = package_name)

  # Roxygen examples blocks:
  ex_start = which(stringr::str_detect(modified_content, "^#'\\s*@examples$"))
  ex_lines = which(stringr::str_detect(modified_content, "^#'"))
  ex_end = ex_lines[
    ex_lines != dplyr::lead(ex_lines, default = .Machine$integer.max) - 1
  ]

  for (i in seq_along(ex_start)) {
    # i=10
    tmp_start = ex_start[i] + 1
    tmp_end = ex_end[ex_end >= tmp_start][1]
    ex_block = modified_content[tmp_start:tmp_end] %>%
      stringr::str_remove("^#'")
    mod_block = .qualify_with_parse_data(
      ex_block,
      functionNames,
      package_name,
      references
    )
    summary = dplyr::bind_rows(summary, mod_block$summary)
    mod_block = paste0("#'", mod_block$modified_content)
    modified_content[tmp_start:tmp_end] = mod_block
  }

  summary = summary %>%
    dplyr::group_by(name, pkg) %>%
    dplyr::summarise(value = sum(value))

  return(list(
    modified_content = modified_content,
    summary = summary
  ))
}

# Example usage:
# functionNames <- c("read_csv", "filter", "mutate")
# content <- c(
#   "data <- read_csv('file.csv')  # Load data",
#   "# filtered <- filter(data, x > 5)  # This is commented",
#   "result <- dplyr::mutate(data, y = x * 2)  # Already qualified",
#   "final <- filter(result, y > 10)"
# )

# Method 2: Using getParseData (more detailed)
# result2 <- qualify_with_parse_data(content, functionNames, "dplyr")

# Test function with enhanced examples
# test_parser_approach <- function() {
#   test_content <- c(
#     "f = function(filter) {",                            #
#     "  data <- read_csv('file.csv')",                    # Function call - needs qualification
#     "  # filtered <- filter(data, x > 5)",               # Commented out - should be unaltered
#     "  result <- dplyr::mutate(data, y = x * 2)",        # Already qualified
#     "  final <- filter(result, y > 10)",                 # Function call - needs qualification
#     "  files <- c('a.csv', 'b.csv', 'c.csv')",           # Variable assignment
#     "  all_data <- lapply(files, read_csv)",             # Function reference - needs qualification
#     "  custom_func <- function(x) filter(x, y > 0)",     # Function call in function definition
#     "  # lapply(files, read_csv)  # Commented function reference", # Commented out - should be unaltered
#     "  qualified_lapply <- lapply(files, readr::read_csv)", # Already qualified reference
#     "}"
#   )
#
#   test_functions <- c("read_csv", "filter", "mutate", "map")
#
#   cat("Original content:\n")
#   cat("================\n")
#   for (i in seq_along(test_content)) {
#     cat(sprintf("%d: %s\n", i, test_content[i]))
#   }
#
#   cat("\nModified content:\n")
#   cat("================\n")
#   result <- .qualify_with_parse_data(test_content, test_functions, "dplyr", references = TRUE)
#   for (i in seq_along(result$modified_content)) {
#     cat(sprintf("%d: %s\n", i, result$modified_content[i]))
#   }
#
#   cat("\nDetailed summary:\n")
#   cat("================\n")
#   print(result$summary)
#
#   return(result)
# }
# #

# TODO: interactive diff viewer?
# https://github.com/muschellij2/diffr/blob/master/R/diffr.R
