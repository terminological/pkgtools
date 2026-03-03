# ---
# destfile: template-`##`-class.R
# last-updated: 2024-03-24
# license: https://unlicense.org
# imports:
#    - knitr
#    - pillar
#    - purrr
#    - rlang
#    - utils
# ---

# TODO: update class specific functions ----

#' Create a ``##`` S3 object
#'
#' TODO: document the purpose of this class, and its internal structure.
#' including any functions attached to it.
#'
#' ``##`` and ``##_list`` objects support `$` access for fields and `@` access
#' for attributes. ``##_list``s can be made with the `c()` or `rep()` functions,
#' or with the `purrr` style map functions, and they support subsetting.
#' Individual ``##`` members of ``##_list``s can be accessed with `[[`.
#'
#' @param x a ``##`` S3 object
#' @param ... passed onto methods
#' @name `s3_##`
#' @keywords internal
NULL

#' Create a new ``##`` S3 object
#' @param value EXAMPLE: a set of named values to use in the ``##`` S3 object.
#' @param .attr EXAMPLE: attributes to assign to the ``##`` S3 object
#' @returns a new ``##`` S3 object
#' @concept `##_s3`
#' @keywords internal
`new_##` = function(value = NA, .attr = list()) {
  # TODO: Implement a constructor method using appropriate parameters
  # and setting the ``##`` class. Document the parameters.
  # All parameters need a default value for an empty ``##``.
  out = structure(list(value = value), class = "##")
  for (attnm in names(.attr)) {
    attr(out, attnm) = .attr[attnm]
  }
  return(out)
}

#' Format a ``##`` S3 object
#' @inheritParams `s3_##`
#' @export
#' @returns a character value
#' @concept `##_s3`
`format.##` = function(x, ...) {
  # TODO: implement custom formatting
  NextMethod("format", x, ...)
}

#' @export
#' @describeIn `as.##` EXAMPLE: description for this
#' @concept `##_s3`
#' @param ex EXAMPLE: parameters used as attributes (not `x` and `...`)
`as.##.numeric` = function(x, ..., ex = "demo") {
  # TODO: Create a set of meaningful `as.##.XXX` methods using the ``new_##`()`
  # new parameters need to be documented, but the overall method does not.
  # if the @describeIn annotation is present
  `new_##`(value = x, .attr = rlang::list2(...))
}

#' @param x a ``##`` S3 class
#' @returns a numeric or other order-able item
#' @noRd
`.sort_by_##` = function(x) {
  # TODO: make specific to the ``##`` class. this is optional. e.g.:
  # stop("`##` is not sortable.")
  return(x$value)
}

#TODO: implement additional object specific exported functions

# Boilerplate generated functions ----

# S3 classes can be used in a  number of styles as described in the `vctrs`
# package. This template provides some basic infrastructure for creating an S3
# `scalar` class style. This is centred around a list of unequal length data, or
# functions, like the output of base `stats::lm`. To make these work in data
# frames we need an ancillary list class that allows the object to be
# incorporated into data frames or operated on by `purrr`. These objects are not
# obviously useful in operations like addition etc., and are just dumb
# structured data holders that behave approximately predictably when combined
# into lists. The list classes are sortable and subset-able.

# S3 `##` class ----

#' Check if this is a ``##`` S3 object
#' @export
#' @param x anything
#' @returns TRUE or FALSE
#' @concept `##_s3`
`is.##` = function(x) {
  return(inherits(x, "##"))
}

#'  Check if this is a ``##_list`` S3 object
#' @param x anything
#' @returns TRUE or FALSE
#' @export
#' @concept `##_s3`
`is.##_list` = function(x) {
  inherits(x, "##_list")
}

#' @inherit `s3_##`
#' @export
#' @returns a ``##`` S3 object
#' @concept `##_s3`
`as.##` = function(x, ...) {
  if (missing(x)) {
    return(`new_##`())
  }
  if (`as.##`(x)) {
    return(x)
  }
  UseMethod("`as.##`", x)
}

#' Concatenate a ``##`` S3 object or ``##_list``s
#' @param ... some of ``##``, ``##_list``s or lists of ``##``
#' @returns a ``##_list``
#' @concept `##_s3`
#' @export
`c.##` = function(...) {
  dots = rlang::list2(...)
  if (length(dots) == 0) {
    return(`as.##_list`(NULL))
  }
  dots[[1]] = `as.##_list`(dots[[1]])
  return(do.call(`c.##_list`, dots))
}

#' Apply a function to each element of a vector returning a ``##_list``
#'
#' Analogous to `purrr::map_dbl()`
#'
#' @inheritParams purrr::map
#' @param .f a function to apply that returns a ``##`` S3 object (usually an
#'   ``as.##()`` call)
#' @seealso [purrr::map()]
#'
#' @returns a ``##_list``
#' @export
#' @concept `##_s3`
`map_##` = function(.x, .f, ..., .progress = FALSE) {
  # This will flatten any nested `##_lists.` This is good as .f may return a
  # single `##` or more likely a 1 element `##_list.`
  return(purrr::map(.x, .f, ..., .progress = .progress) %>% `as.##_list`())
}

#' Map over two inputs returning a ``##_list``
#'
#' Analogous to `purrr::map2_dbl()`
#'
#' @inheritParams purrr::map2
#' @param .f a function to apply to each `.x`, `.y` pair that returns a ``##``
#'   S3 object (usually an ``as.##()`` call)
#' @seealso [purrr::map2()]
#'
#' @returns a ``##_list``
#' @export
#' @concept `##_s3`
`map2_##` = function(.x, .y, .f, ..., .progress = FALSE) {
  return(
    purrr::map2(.x, .y, .f, ..., .progress = .progress) %>% `as.##_list`()
  )
}

#' Map over multiple inputs returning a ``##_list``
#'
#' Analogous to `purrr::pmap_dbl()`
#'
#' @inheritParams purrr::pmap
#' @param .f a function to apply to each `.l` item (usually an ``as.##()`` call)
#' @seealso [purrr::map()]
#' @returns a ``##_list``
#' @export
#' @concept `##_s3`
`pmap_##` = function(.l, .f, ..., .progress = FALSE) {
  return(
    purrr::pmap(
      .l,
      .f,
      ...,
      .progress = .progress
    ) %>%
      `as.##_list`()
  )
}

#' Create an empty ``##_list``
#' @inherit `s3_##_common`
#' @export
#' @returns an empty ``##_list``
#' @concept `##_s3`
`##` = function() {
  return(`as.##_list`(NULL))
}

## Utility functions ----

# removes this class from the object - internal use
`.unclass_##` = function(x) {
  class(x) <- setdiff(class(x), "##")
  return(x)
}

# x is a list but not a `##`. This means it is a plain list
# or a `##_list`
`.is_list_excl_##` = function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (`is.##`(x)) {
    return(FALSE)
  }
  return(TRUE)
}

## Hidden exported functions ----

#' ``##`` S3 Class operations
#'
#' Boilerplate S3 class operation
#'
#' @param x a ``##`` S3 object
#' @param ... passed onto methods
#' @name `s3_##_common`
#' @keywords internal
NULL


#' Default method throws error
#' @export
#' @inherit `s3_##_common`
#' @returns nothing
#' @concept `##_s3`
#' @keywords internal
`as.##.default` = function(x, ...) {
  if (`is.##`(x)) {
    return(x)
  }
  stop("Don't know how to convert a `", class(x)[[1]], "` to a ``##``.")
}

#' Cast a ``##`` S3 object to a character vector
#' @inherit `s3_##_common`
#' @export
#' @returns a character vector
#' @concept `##_s3`
#' @keywords internal
`as.character.##` = function(x, ...) {
  return(format(x, ...))
}

#' Print a ``##`` S3 object
#' @inherit `s3_##_common`
#' @export
#' @returns nothing
#' @concept `##_s3`
#' @keywords internal
`print.##` = function(x, ...) {
  cat(suppressWarnings(format(x, ...)))
  invisible(NULL)
}

#' Print a ``##`` S3 object in an Rd document
#' @exportS3Method knitr::knit_print `##`
#' @inherit `s3_##_common`
#' @returns an `as-is` knitr chunk
#' @concept `##_s3`
#' @keywords internal
`knit_print.##` = function(x, ...) {
  structure(format(x), class = "knit_asis")
}

#' ``##`` S3 objects length is always 1
#' @export
#' @inherit `s3_##_common`
#' @returns 1 always
#' @concept `##_s3`
#' @keywords internal
`length.##` = function(x, ...) {
  return(1)
}

#' Pillar type name
#' @exportS3Method pillar::type_sum `##`
#' @inherit `s3_##_common`
#' @returns a string
#' @concept `##_s3`
#' @keywords internal
`type_sum.##` = function(x, ...) {
  abbreviate("##", 3)
}


#' Extract named attribute from a ``##``
#' @inherit `s3_##_common`
#' @param y item to retrieve
#' @returns an attribute value for `x`
#' @export
#' @concept `##_s3`
#' @keywords internal
#' @name `at.##`
`@.##` = function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab = deparse(substitute(y))
  }
  return(attr(x, ylab))
}

#' Support for auto suggests on ``##_list``s
#' @inherit `s3_##_common`
#' @param pattern a regular expression
#' @returns the names of the attributes
#' @exportS3Method utils::.AtNames `##`
#' @concept `##_s3`
#' @keywords internal
`.AtNames.##` = function(x, pattern) {
  return(utils::.DollarNames(attributes(x), pattern))
}


#' Repeat an ``##`` S3 object
#' @inherit `s3_##_common`
#' @inheritDotParams base::rep -x
#' @returns a ``##_list``
#' @export
#' @concept `##_s3`
#' @keywords internal
`rep.##` = function(x, ...) {
  rep(`as.##_list`(x), ...)
}

#' Convert a ``##`` S3 object into a plain list
#' @inherit `s3_##_common`
#' @export
#' @returns the internal structure of the object as a plain list
#' @concept `##_s3`
#' @keywords internal
`as.list.##` = function(x, ...) {
  `.unclass_##`(x)
}

# S3 `##`_list class ----

## Hidden exported functions ----

#' Manipulate ``##`` S3 object lists
#'
#' These boilerplate functions allow generic list behaviour from ``##_list``
#' classes allowing ``##`` S3 objects to be used in lists or dataframes.
#'
#' @param x a ``##_list`` S3 object
#' @param ... passed onto methods
#'
#' @name `s3_##_list`
#' @keywords internal
NULL

#' Un format the ``##_list``
#' @inherit `s3_##_list`
#' @export
#' @concept `##_s3`
#' @keywords internal
#' a plain list of ``##`` S3 objects
`as.list.##_list` = function(x, ...) {
  unclass(x)
}

#' Length of a ``##_list``
#' @inherit `s3_##_list`
#' @export
#' @returns the length
#' @concept `##_s3`
#' @keywords internal
`length.##_list` = function(x, ...) {
  return(length(unclass(x)))
}


#' Cast to a list of ``##`` S3 objects
#'
#' This function wraps ``##`` and unwraps plain lists such that
#' the result is a flat ``##_list`` containing ``##`` objects only
#'
#' @inherit `s3_##_list`
#' @return a ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`as.##_list` = function(x) {
  if (length(unlist(x)) == 0) {
    # .class may be asserted when creating zero size / NULL ``##``s
    return(structure(list(), class = c("##_list", "list")))
  }
  if (`is.##`(x)) {
    return(structure(list(x), class = c("##_list", "list")))
  }
  if (is.list(x)) {
    # x is a list or `##_list` (but cannot be a single `##` at this point)
    while (any(sapply(x, `.is_list_excl_##`))) {
      # if there are any nested `##_lists` or plain lists we will collapse
      # them . A `##_list` must be a list of `##`s without hierarchy.
      # We also have to make sure that plain `##`s are wrapped
      x = lapply(x, `as.##_list`)
      x = unlist(x, recursive = FALSE)
    }
    return(structure(x, class = c("##_list", "list")))
  }

  stop(
    "Not convertible to a ``##_list`` x is not ``##_list``, a ``##`` or a uniform list of ``##``s",
    call. = FALSE
  )
}

#' Format a ``##_list``
#' @inherit `s3_##_list`
#' @export
#' @returns a character vector for the list
#' @concept `##_s3`
#' @keywords internal
`format.##_list` = function(x, ...) {
  unlist(lapply(x, format))
}

#' Print a ``##_list``
#' @inherit `s3_##_list`
#' @export
#' @returns nothing
#' @concept `##_s3`
#' @keywords internal
`print.##_list` = function(x, ...) {
  cat(sprintf("`##`(%s)\n", length(x)))
  cat(suppressWarnings(`format.##_list`(x, ...)), "\n")
  invisible(NULL)
}

#' Convert a ``##_list`` to character
#' @inherit `s3_##_list`
#' @export
#' @returns a character vector
#' @concept `##_s3`
#' @keywords internal
`as.character.##_list` = function(x, ...) {
  `format.##_list`(x, ...)
}

#' Pillar type name
#' @inherit `s3_##_list`
#' @exportS3Method pillar::type_sum `##_list`
#' @concept `##_s3`
#' @returns a name
#' @keywords internal
`type_sum.##_list` = function(x, ...) {
  I(sprintf("<%s[]>", abbreviate("##", 3, named = FALSE)))
}

#' Pillar shaft function
#' @inherit `s3_##_list`
#' @returns a pillar shaft
#' @exportS3Method pillar::pillar_shaft
#' @concept `##_s3`
#' @keywords internal
`pillar_shaft.##_list` <- function(x, ...) {
  out <- `format.##_list`(x, ...)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' Concatenate or construct a ``##_list``
#'
#' @param ... some of ``##_list`` and ``##`` or list of ``##``s
#' @returns a flattened ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`c.##_list` = function(...) {
  dots = rlang::list2(...)
  if (`is.##_list`(dots)) {
    return(dots)
  }
  if (length(dots) == 1) {
    return(`as.##_list`(dots))
  }
  # remove empty items
  dots = dots[sapply(dots, length) > 0]
  # make sure all list entries are a `##` list (dots is list of `##_list`)
  tmp = lapply(dots, `as.##_list`)
  # convert to plain list of lists
  tmp = lapply(tmp, `as.list.##_list`)
  # collapse one level
  tmp = unlist(tmp, recursive = FALSE)
  # convert to `##_list.` this should throw an error if types are mixed.
  return(`as.##_list`(tmp))
}

#' Repeat a ``##_list``
#' @inherit `s3_##_list`
#' @inheritDotParams base::rep -x
#' @returns a ``##_list`` S3 object
#' @concept `##_s3`
#' @export
#' @keywords internal
`rep.##_list` = function(x, ...) {
  tmp = NextMethod()
  return(`as.##_list`(tmp))
}


#' Sort a ``##_list``
#' @inherit `s3_##_list`
#' @param decreasing reverse the sort order
#' @returns an ordered ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`sort.##_list` = function(x, decreasing = FALSE, ...) {
  indx = order(unlist(sapply(x, `.sort_by_##`)))
  if (decreasing) {
    indx = rev(indx)
  }
  return(x[indx])
}

### `##_list` Subsetting functions ----

#' Extract named item(s) from a ``##_list``
#' @inherit `s3_##_list`
#' @param y item to retrieve
#' @returns a list or vector of the named items
#' @export
#' @concept `##_s3`
#' @keywords internal
`$.##_list` = function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab = deparse(substitute(y))
  }
  if (length(x) == 1) {
    return(x[[1]][[ylab]])
  }
  return(sapply(seq_along(x), function(i) x[[i]][[ylab]], USE.NAMES = FALSE))
}

#' Support for auto suggests on ``##_list``s
#' @inherit `s3_##_list`
#' @returns the names of the children
#' @exportS3Method utils::.DollarNames `##_list`
#' @concept `##_s3`
#' @keywords internal
`.DollarNames.##_list` = function(x, pattern) {
  if (length(x) == 0) {
    return(character())
  }
  return(utils::.DollarNames(x[[1]], pattern))
}

#' Extract named item(s) from a ``##_list``
#' @inherit `s3_##_list`
#' @param y attribute to retrieve
#' @returns a vector or list of the underlying ``##`` attribute values
#' @export
#' @concept `##_s3`
#' @keywords internal
#' @name `at.##_list`
`@.##_list` = function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab = deparse(substitute(y))
  }
  if (length(x) == 1) {
    return(attr(x[[1]], ylab))
  }
  return(unname(sapply(
    seq_along(x),
    function(i) attr(x[[i]], ylab),
    USE.NAMES = FALSE
  )))
}

#' Support for auto suggests on ``##_list``s
#' @inherit `s3_##_list`
#' @returns the names of the attributes
#' @exportS3Method utils::.AtNames `##_list`
#' @concept `##_s3`
#' @keywords internal
`.AtNames.##_list` = function(x, pattern) {
  if (length(x) == 0) {
    return(character())
  }
  return(utils::.DollarNames(attributes(x[[1]]), pattern))
}

#' Subset a ``##_list``
#' @inherit `s3_##_list`
#' @returns a ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`[.##_list` = function(x, ...) {
  y = `[`(unclass(x), ...)
  return(`as.##_list`(y))
}

#' Assign a subset to a ``##_list``
#' @inherit `s3_##_list`
#' @param value the value as ``##_list`` or ``##`` S3 objects
#' @returns the updated ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`[<-.##_list` = function(x, ..., value) {
  if (!`is.##_list`(value) && !`is.##`(value)) {
    stop(
      "cannot add a `",
      class(value)[[1]],
      "` to a ``##_list``"
    )
  }
  y = `[<-`(unclass(x), ..., value)
  return(`as.##_list`(y))
}

#' get a value from a ``##_list``
#' @inherit `s3_##_list`
#' @returns a ``##`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`[[.##_list` = function(x, ...) {
  y = `[[`(unclass(x), ...)
  return(y)
}

#' set a single value in a ``##_list``
#' @inherit `s3_##_list`
#' @param value the value
#' @returns the updated ``##_list`` S3 object
#' @export
#' @concept `##_s3`
#' @keywords internal
`[[<-.##_list` = function(x, ..., value) {
  if (`is.##_list`(value) && length(value) == 1) {
    value = value[[1]]
  }
  if (!`is.##`(value)) {
    stop(
      "cannot add a `",
      class(value)[[1]],
      "` to a ``##_list``"
    )
  }
  y = `[[<-`(unclass(x), ..., value)
  return(`as.##_list`(y))
}
