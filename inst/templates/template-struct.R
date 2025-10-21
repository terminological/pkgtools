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

# S3 classes can be used in a  number of styles as described in the `vctrs`
# package. This template provides some basic infrastructure for creating an S3
# `scalar` class style. This is centred around a list of unequal length data, or
# functions like the base `stats::lm`. To make these work in data frames we need
# an ancillary list class that allows the object to be incorporated into data
# frames or operated on by `purrr`. These objects are not obviously useful in
# operations like addition etc., and are just dumb structured data holders that
# behave approximately predictably when combined into lists. The list classes
# are sortable and subset-able.

#' ``##`` S3 Class operations
#'
#' TODO: document the purpose of this class, and its internal structure.
#' including any functions attached to it.
#'
#' @param x a ``##`` S3 object
#' @param ... passed onto methods
#' @name `s3_##`
NULL

#' @describeIn `s3_##` Create a new ``##`` S3 object
#' @param value EXAMPLE: a set of named values to use in the ``##`` S3 object.
#' @param .attr EXAMPLE: attributes to assign to the ``##`` S3 object
#' @returns a new ``##`` S3 object
#' @keywords internal
#' @concept `##_s3`
`new_##` = function(value, .attr = list()) {
  # TODO: Implement a constructor method using appropriate parameters
  # and setting the ``##`` class. Document the parameters.
  out = structure(list(value = value), class = "##")
  for (attnm in names(.attr)) {
    attr(out, attnm) = .attr[attnm]
  }
  return(out)
}

#' @describeIn `s3_##` Format a ``##`` S3 object
#' @export
#' @keywords internal
#' @returns a character value
#' @concept `##_s3`
`format.##` = function(x, ...) {
  # TODO: implement custom formatting
  NextMethod("format", x, ...)
}

#' @export
#' @concept `##_s3`
#' @param ... EXAMPLE: additional parameters used as attributes
`as.##.numeric` = function(x, ...) {
  # TODO: Create a set of meaningful `as.##.XXX` methods using the ``new_##`()`
  # new parameters need to be documented, but the overall method does not.
  `new_##`(value = x, .attr = rlang::list2(...))
}

#' @param x a ``##`` S3 class
#' @returns a numeric or other orderable item
#' @noRd
`.order_##` = function(x) {
  # TODO: make specific to the ``##`` class:
  return(x$value)
}

#TODO: implement additional object specific exported functions

# S3 `##` class ----

#' @describeIn `s3_##_list` Create an empty ``##_list``
#' @export
#' @returns an empty ``##_list``
#' @concept `##_s3`
`##` = function() {
  return(`as.##_list`(NULL))
}

# removes this class from the object - internal use
`.unclass_##` = function(x) {
  class(x) <- setdiff(class(x), "##")
  return(x)
}


#' @describeIn `s3_##` Cast to a ``##``
#' @export
#' @returns a ``##`` S3 object
#' @concept `##_s3`
`as.##` = function(x, ...) {
  UseMethod("`as.##`", x)
}

#' @export
#' @concept `##_s3`
`as.##.default` = function(x, ...) {
  if (`is.##`(x)) {
    return(x)
  }
  stop("Don't know how to convert a `", class(x)[[1]], "` to a ``##``.")
}

#' @describeIn `s3_##` Cast a ``##`` S3 object to a character vector
#' @export
#' @keywords internal
#' @returns a character vector
#' @concept `##_s3`
`as.character.##` = function(x, ...) {
  return(format(x, ...))
}

#' @describeIn `s3_##` Print a ``##`` S3 object
#' @export
#' @keywords internal
#' @returns nothing
#' @concept `##_s3`
`print.##` = function(x, ...) {
  cat(suppressWarnings(format(x, ...)))
  invisible(NULL)
}

#' @describeIn `s3_##` Print a ``##`` S3 object in an Rd document
#' @exportS3Method knitr::knit_print `##`
#' @keywords internal
#' @returns an `as-is` knitr chunk
#' @concept `##_s3`
`knit_print.##` = function(x, ...) {
  structure(format(x), class = "knit_asis")
}

#' @describeIn `s3_##` ``##`` S3 objects length is always 1
#' @export
#' @keywords internal
#' @returns 1 always
#' @concept `##_s3`
`length.##` = function(x, ...) {
  return(1)
}

#' @exportS3Method pillar::type_sum `##`
`type_sum.##` = function(x, ...) {
  abbreviate("##", 3)
}

#' @describeIn `s3_##` Check is a ``##`` S3 object
#' @export
#' @returns TRUE or FALSE
#' @concept `##_s3`
`is.##` = function(x) {
  return(inherits(x, "##"))
}

#' @describeIn `s3_##` Extract named attribute from a ``##``
#' @param y item to retrieve
#' @returns an attribute value for `x`
#' @export
`@.##` = function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab = deparse(substitute(y))
  }
  return(attr(x, ylab))
}

#' @describeIn `s3_##` Support for auto suggests on ``##_list``s
#' @param pattern a regular expression
#' @returns the names of the attributes
#' @export
`.AtNames.##` = function(x, pattern) {
  return(.DollarNames(attributes(x), pattern))
}

#' @describeIn `s3_##` Concatenate a ``##`` S3 object or ``##_list``s
#' @param ... some of ``##``, ``##_list``s or lists of ``##``
#' @returns a ``##_list``
#' @export
`c.##` = function(...) {
  dots = rlang::list2(...)
  if (length(dots) == 0) {
    return(`as.##_list`(NULL))
  }
  dots[[1]] = `as.##_list`(dots[[1]])
  return(do.call(`c.##_list`, dots))
}

#' @describeIn `s3_##` Repeat an ``##`` S3 object
#' @returns a ``##_list``
#' @export
`rep.##` = function(x, ...) {
  rep(`as.##_list`(x), ...)
}

#' @describeIn `s3_##` Convert a ``##`` S3 object into a plain list
#' @export
#' @returns the internal structure of the object as a plain list
#' @concept `##_s3`
`as.list.##` = function(x, ...) {
  `.unclass_##`(x)
}

# S3 `##`_list class ----

#' Manipulate ``##`` S3 object lists
#'
#' These functions allow generic list behaviour.
#'
#' @param x a ``##_list`` S3 object
#' @param ... passed onto methods
#'
#' @concept `##_s3`
#' @name `s3_##_list`
NULL

#' @describeIn `s3_##_list` Unformat the ``##_list``
#' @export
#' @concept `##_s3`
#' a plain list of ``##`` S3 objects
`as.list.##_list` = function(x, ...) {
  unclass(x)
}

#' @describeIn `s3_##_list` Length of a ``##_list``
#' @export
#' @returns the length
#' @concept `##_s3`
`length.##_list` = function(x, ...) {
  return(length(unclass(x)))
}

#' @describeIn `s3_##_list` Check is a ``##_list``
#' @export
#' @concept `##_s3`
`is.##_list` = function(x) {
  inherits(x, "##_list")
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

#' Cast to a list of ``##`` S3 objects
#'
#' This function wraps ``##`` and unwraps plain lists such that
#' the result is a flat ``##_list`` containing ``##`` objects only
#'
#' @param x a list
#' @return a ``##_list`` S3 object
#' @export
#' @concept `##_s3`
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

#' @describeIn `s3_##_list` Format a ``##_list``
#' @export
#' @returns a character vector for the list
#' @concept `##_s3`
`format.##_list` = function(x, ...) {
  unlist(lapply(x, format))
}

#' @describeIn `s3_##_list` Print a ``##_list``
#' @export
#' @returns nothing
#' @concept `##_s3`
`print.##_list` = function(x, ...) {
  cat(sprintf("`##`(%s)\n", length(x)))
  cat(suppressWarnings(`format.##_list`(x, ...)), "\n")
  invisible(NULL)
}

#' @describeIn `s3_##_list` Convert a ``##_list`` to character
#' @export
#' @returns a character vector
#' @concept `##_s3`
`as.character.##_list` = function(x, ...) {
  `format.##_list`(x, ...)
}

#' @exportS3Method pillar::type_sum `##_list`
#' @concept `##_s3`
`type_sum.##_list` = function(x, ...) {
  I(sprintf("<%s[]>", abbreviate("##", 3, named = FALSE)))
}

#' @exportS3Method pillar::pillar_shaft
#' @concept `##_s3`
`pillar_shaft.##_list` <- function(x, ...) {
  out <- `format.##_list`(x, ...)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @describeIn `s3_##_list` Concatenate or construct a ``##_list``
#' @param ... some of ``##_list`` and ``##`` or list of ``##``s
#' @returns a fully flattened ``##_list`` S3 object
#' @export
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

#' @describeIn `s3_##_list` Repeat a ``##_list``
#' @export
`rep.##_list` = function(x, ...) {
  tmp = NextMethod()
  return(`as.##_list`(tmp))
}


#' @describeIn `s3_##_list` Repeat a ``##_list``
#' @param decreasing reverse the sort order
#' @export
`sort.##_list` = function(x, decreasing = FALSE, ...) {
  indx = order(unlist(sapply(x, `.order_##`)))
  if (decreasing) {
    indx = rev(indx)
  }
  return(x[indx])
}

## `##_list` Subsetting functions ----

#' @describeIn `s3_##_list` Extract named item(s) from a ``##_list``
#' @param y item to retrieve
#' @export
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
#' @keywords internal
#' @param x a ``##_list``
#' @returns the names of the children
#' @export
`.DollarNames.##_list` = function(x, pattern) {
  if (length(x) == 0) {
    return(character())
  }
  return(.DollarNames(x[[1]], pattern))
}

#' @describeIn `s3_##_list` Extract named item(s) from a ``##_list``
#' @param y attribute to retrieve
#' @returns a vector or list of the underlying ``##`` attribute values
#' @export
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
#' @keywords internal
#' @param x a ``##_list``
#' @returns the names of the attributes
#' @export
`.AtNames.##_list` = function(x, pattern) {
  if (length(x) == 0) {
    return(character())
  }
  return(.DollarNames(attributes(x[[1]]), pattern))
}

#' @describeIn `s3_##_list` Subset a ``##_list``
#' @returns a ``##_list`` S3 object
#' @export
`[.##_list` = function(x, ...) {
  y = `[`(unclass(x), ...)
  return(`as.##_list`(y))
}

#' @describeIn `s3_##_list` Assign a subset to a ``##_list``
#' @param value the value as ``##_list`` or ``##`` S3 objects
#' @returns the updated ``##_list`` S3 object
#' @export
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

#' @describeIn `s3_##_list` get a value from a ``##_list``
#' @returns a ``##`` S3 object
#' @export
`[[.##_list` = function(x, ...) {
  y = `[[`(unclass(x), ...)
  return(y)
}

#' @describeIn `s3_##_list` set a single value in a ``##_list``
#' @param value the value
#' @returns the updated ``##_list`` S3 object
#' @export
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
