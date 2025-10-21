#' Style code in `pkgtools` style
#'
#' This is adapted from `https://github.com/gadenbuie/grkstyle`
#' It differs in that it will break apart long lines regardless of their initial
#' line breaks to try and fit them into a set width. It does not unbreak lines.
#'
#' @inheritParams styler::style_text
#' @inheritDotParams styler::style_text -text -transformers
#' @inheritDotParams pkgtools_style_transformer
#'
#' @returns the formatted code
#' @concept edit
#' @export
style_text = function(
  text,
  ...,
  transformers = pkgtools_style_transformer(...)
) {
  return(styler::style_text(
    text,
    ...,
    transformers = transformers
  ))
}


#' A code transformer for use with [styler::style_text()]
#'
#' This transformer is adapted from `https://github.com/gadenbuie/grkstyle`
#' It differs in that it will break apart long lines regardless of their initial
#' line breaks to try and fit them into a set width. It does not unbreak lines.
#'
#' @param width the max width of the code (excludes indent hence approximate)
#' @keywords internal
#'
#' @unit
#' # style_text('testthat::expect_equal(iris[1, ], structure(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2, Species = structure(1L, levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1L, class = "data.frame"))')
#' # style_text('testthat::expect_equal(iris[1, ], structure(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2, Species = structure(1L, levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1L, class = "data.frame"))', width=80)
pkgtools_style_transformer <- function(
  ...,
  width = 70
) {
  use_tabs <- use_tabs()

  tidy_style <- styler::tidyverse_style(
    indent_by = use_tabs$indent_by
  )
  tidy_style$indent_character <- use_tabs$indent_character
  tidy_style$style_guide_name <- "pkgtools::pkgtools_style_transformer@https://github.com/terminological"
  tidy_style$style_guide_version <- pkgload::pkg_version(system.file(
    package = "pkgtools"
  ))

  # line breaks between *all* arguments if line breaks between *any*
  tidy_style$line_break$set_linebreak_each_argument_if_multi_line <- function(
    pd
  ) {
    if (!(any(pd$token == "','"))) {
      return(pd)
    }
    # does this expression contain expressions with linebreaks?
    has_children <- purrr::some(pd$child, purrr::negate(is.null))
    has_internal_linebreak <- FALSE
    is_function_definition <- pd$token[1] == "FUNCTION"
    if (has_children && !is_function_definition) {
      children <- pd$child

      # don't count anything inside {} as internal linebreaks
      idx_pre_open_brace <- which(pd$token_after == "'{'")
      if (length(idx_pre_open_brace)) {
        children[idx_pre_open_brace + 1] <- NULL
      }

      has_internal_linebreak <- children %>%
        purrr::discard(is.null) %>%
        purrr::map_lgl(function(x) {
          sum(x$newlines, x$lag_newlines) > 0
        }) %>%
        any()
    }

    # predict linebreak based on widths of children when broken or unbroken
    max_len = function(pd, firstline, width) {
      mx = mx_brkn = cur = cur_brkn = 0
      for (i in seq_len(nrow(pd))) {
        if (pd$newlines[i] > 0) {
          # reset line counters
          cur_brkn = cur = 2 - firstline # indent newline - hanging indent
        }
        if (is.null(pd$child[[i]])) {
          # this is a terminal node with no sub structure
          piece_size = stringr::str_length(pd$text[i]) + pd$spaces[i]
          cur = cur + piece_size
          cur_brkn = cur_brkn + piece_size
          if (mx < cur) {
            mx = cur
          }
          if (mx_brkn < cur_brkn) {
            mx_brkn = cur_brkn
          }
          if (pd$token[i] == "','") {
            # This position could be broken if this level were split
            cur_brkn = 2 - firstline
          }
          if (pd$lag_newlines[i]) {
            cur_brkn = cur = 2 - firstline
          }
        } else {
          # This is a child node. What is the max length of the child.
          # depends on whether this node is broken because of hanging indent.
          cur = cur +
            max_len(pd$child[[i]], stringr::str_length(cur), width)$width
          cur_brkn = cur_brkn +
            max_len(pd$child[[i]], stringr::str_length(cur_brkn), width)$width
        }
      }
      if (mx > width) {
        # unbroken line exceeds width.
        return(list(brk = TRUE, width = mx_brkn))
      } else {
        return(list(brk = FALSE, width = mx))
      }
    }

    # debug(max_len)
    sublen = max_len(pd, firstline = 0, width = width)
    # message(sublen$width, " ", sublen$brk, " ", paste0(pd$text, collapse = ""))
    # browser(text = paste0(pd$text, collapse = ""))

    if (sum(pd$newlines, pd$lag_newlines) < 1 && !sublen$brk) {
      return(pd)
    }

    idx_comma <- which(pd$token == "','")
    idx_open_paren <- grep("'[[(]'", pd$token)
    idx_close_paren <- grep("'(]|\\))'", pd$token)
    idx_comma_has_comment <- which(pd$token[idx_comma + 1] == "COMMENT")
    idx_comma[idx_comma_has_comment] <- idx_comma[idx_comma_has_comment] + 1
    pd[idx_comma + 1L, "lag_newlines"] <- 1L
    if (length(idx_open_paren)) {
      pd[idx_open_paren[1] + 1L, "lag_newlines"] <- 1L
    }
    if (length(idx_close_paren)) {
      pd[idx_close_paren[length(idx_close_paren)], "lag_newlines"] <- 1L
    }
    pd
  }

  # Function arguments on new lines, indented with 2 spaces
  tidy_style$indention$update_indention_ref_fun_dec <- function(pd_nested) {
    if (pd_nested$token[1] == "FUNCTION" && nrow(pd_nested) > 4) {
      seq <- seq.int(3L, nrow(pd_nested) - 2L)
      pd_nested$indention_ref_pos_id[seq] <- 0L
      pd_nested$indent[seq] <- use_tabs$indent_by
    }
    pd_nested
  }

  # Disable styler's extra indentation of closing `) {` in function declarations
  # https://github.com/gadenbuie/grkstyle/issues/10
  styler_unindent_fun_dec <- tidy_style$indention$unindent_fun_dec
  tidy_style$indention$unindent_fun_dec <- function(pd) {
    styler_unindent_fun_dec(pd, indent_by = 0)
  }

  tidy_style
}

use_tabs <- function(use_tabs = NULL) {
  if (is.null(use_tabs)) {
    root <- tryCatch(
      rprojroot::find_rstudio_root_file(),
      error = function(...) NULL
    )

    if (is.null(root)) {
      return(use_tabs(TRUE))
    }

    rproj <- file.path(root, dir(root, pattern = "[.]Rproj$"))
    rproj <- readLines(rproj, warn = FALSE)

    if (any(grepl("^UseSpacesForTab: No$", rproj))) {
      return(use_tabs(TRUE))
    }

    indent_by_chr <- grep("^NumSpacesForTab: \\d+", rproj, value = TRUE)
    if (!length(indent_by_chr)) {
      return(use_tabs(FALSE))
    }

    indent_by <- as.numeric(sub("NumSpacesForTab: ", "", indent_by_chr))
    return(use_tabs(list(indent_by = indent_by, indent_character = " ")))
  }

  if (isTRUE(use_tabs)) {
    use_tabs <- list(indent_by = 1L, indent_character = "\t")
  } else if (identical(use_tabs, FALSE)) {
    use_tabs <- list(indent_by = 2L, indent_character = " ")
  } else if (!setequal(c("indent_by", "indent_character"), names(use_tabs))) {
    stop(
      "`use_tabs` should be one of `TRUE`, `FALSE`, or a list containing ",
      "items 'intent_by' or 'indent_character'."
    )
  }
  use_tabs
}
