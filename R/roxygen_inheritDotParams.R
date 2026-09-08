#' Use this roclet for pkgtools tags and extensions
#'
#' To use this `pkgtools_rd` roclet, add this to `DESCRIPTION`:
#'
#' ```
#' Roxygen: { library(pkgtools); list(markdown = TRUE,
#'     roclets = c("collate", "namespace", "pkgtools_rd")) }
#' ```
#'
#' @importFrom roxygen2 roclet
#' @export
#'
#' @examples
#' # An example function definition:
#' fn_definition = "
#'
#' #' foo function
#' #'
#' #' @param foo_inherited The first level inherited from foo
#' #' @param ... not inherited dots in foo.
#' foo = function(foo_inherited, ...) {}
#'
#' #' baz function
#' #'
#' #' @param baz_param a parameter in baz
#' #' @inheritDotParams bar
#' #' @inheritDotParams ggplot2::geom_bar
#' #'
#' baz = function(baz_param) {}
#'
#' #'
#' #' bar function
#' #'
#' #' @param bar_inherited The Second level inherited from bar
#' #' @inheritDotParams foo
#' bar = function(bar_inherited, ...) {}
#' "
#'
#' # For this example we manually parse the function specification in `fn_definition`
#' # creating a .Rd block - normally this is done by `roxygen2` which then
#' # writes this to an .Rd file. This function is not intended to be used
#' # outside of a call to `devtools::document`.
#'
#'
#' roxygen2::roc_proc_text(pkgtools_rd(), fn_definition)
#'
#' # tmp = roxygen2::parse_text(fn_definition)
#' # print(tmp[[1]])
#'
pkgtools_rd = function() {
  # These are classes suffixes. The roclet will have classes roclet_pkgtools_rd,
  # and roclet_rd.
  roxygen2::roclet(c("pkgtools_rd", "rd"))
}

# Can ignore x its only for dispatch.
# roclet_preprocess(x, blocks, base_path)
# roclet_process(x, blocks, env, base_path)
# roclet_output(x, results, base_path, ...)

#' @importFrom roxygen2 roclet_process
#' @export
roclet_process.roclet_pkgtools_rd = function(x, blocks, env, base_path) {
  # Hook to modify or add in tags that don't exist in the
  # blocks <- lapply(blocks, rewrite_examples_if, env, base_path)

  inheritDots = sapply(blocks, function(b) any(sapply(b$tags, function(t) t$tag == "inheritDotParams")))


  # No arguments inherited from `geom_events()`.
  # ✖ In topic 'plot_counts': @inheritDotParams failed.
  topics = withCallingHandlers(
    NextMethod(),
    message = function(x) {
      if (grepl(
        pattern = "inheritDotParams",
        as.character(x)
      )) {
        rlang::cnd_muffle(x)
      }
    }
  )

  topicNames = names(topics)
  # Clear default roclet inherits `...` output:
  topics = lapply(seq_along(topics), function(i) {
    topic = topics[[i]]
    if (inheritDots[i]) {
      suppressWarnings({
        topic$sections$param$value$`...` = NULL
      })
    }
    return(topic)
  })
  # Three levels of dots parameter inheritance
  for (depth in 1:3) {
    lapply(topics, inherit_dot_params, topics, env)
  }
  names(topics) = topicNames
  return(topics)
}

#' @importFrom roxygen2 roclet_output
#' @export
roclet_output.roclet_pkgtools_rd = function(x, results, base_path, ...) {
  pkg = roxygen2::roxy_meta_get("current_package")
  # if (!is.null(pkg) && !is.null(roxy_themes[[pkg]])) {
  #   results <- lapply_with_names(results, add_styles)
  # }

  # TODO: Output the test_that unit tests.
  # write_testthat(results, base_path)

  # Post process the results of the roclet - in this case it is the same as the RD
  # so results should be a named list of RoxyTopic
  # results = update_dot_params(results, base_path)

  # cli::cli_inform("Dispatching to roclet_rd")

  # results must be a named list of RoxyTopics for dispatch to work here.
  NextMethod()
}

# No-op at present
# update_dot_params = function(results, base_path) {
#   return(results)
# }

# topics is a list of RoxyTopic (not a RoxyTopics object
# which is not exported from roxygen2)
inherit_dot_params = function(topic, topics, env) {
  inheritors = topic$get_value("inherit_dot_params")

  if (is.null(inheritors)) {
    return()
  }

  # Need to find formals for each source
  # funs = lapply(inheritors$source, function(x) {
  #   eval(parse(text = x), envir = env)
  # })

  # Then pull out the ones we need
  docs = lapply(
    inheritors$source,
    find_params,
    topics = topics,
    source = topic$get_name()
  )


  # Transitive inheritance of parameter documentation
  # Basically this looks into parent documentation
  # and finds anything that has not been documented already

  sources = inheritors$source
  dests = sapply(sources, resolve_qualified_link)
  defined_params = get_documented_params(topic)

  # docs_selected is a complex list. within it we need to find things that
  # we have not already documented. We have to match by name with the
  # complication that the names might be vectors themselves

  # to_include = list()
  dots_rd = character()

  i = 1
  for (doc in docs) {
    source = sources[[i]]
    dest = dests[[i]]
    # The parameters provided to the @inheritDotParams argument
    inheritParams = unlist(strsplit(inheritors$args[[i]], "\\s+"))
    # The parameters starting with "-"
    removeParams = substr(
      inheritParams[substr(inheritParams, 1, 1) == "-"],
      2,
      1000
    )
    # The parameters not starting with "-" or all of the
    # documented parameters in this RD. (this can include S3 specific
    # documentation in things like dply::mutate)
    keepParams = inheritParams[substr(inheritParams, 1, 1) != "-"]
    if (length(keepParams) == 0) {
      keepParams = unlist(sapply(doc, function(x) x$name))
      if (length(setdiff(removeParams, keepParams)) > 0) {
        warn_roxy_topic(
          topic$get_name(),
          sprintf(
            "@inheritsDotsParam refers to unknown variables: %s",
            paste0(setdiff(removeParams, keepParams), collapse = ", ")
          )
        )
      }
    }

    keepParams = setdiff(keepParams, removeParams)
    keepParams = setdiff(keepParams, defined_params)
    keepParams = c(keepParams, "...")

    # source_entries = list()
    doc_rd = character()
    for (entry in doc) {
      # extras are the documented entries that are not already defined
      # entry$name can be multiple names
      extras = entry$name[entry$name %in% keepParams]
      if (length(extras) > 0) {
        # keep the documentation for these extra parameters
        # this is formatted as a csv for ease
        extras = paste0(extras, collapse = ",")
        # source_entries[[extras]] = entry$value
        doc_rd = c(
          doc_rd,
          sprintf("\\item{\\code{%s}}{%s}\n", extras, entry$value)
        )
      }
    }
    # to_include[[source]] = source_entries
    if (length(doc_rd) > 0) {
      dots_rd = c(
        dots_rd,
        sprintf(
          "\n  Additional named arguments passed on to \\code{\\link[%s]{%s}}\\describe{\n    %s}",
          dest,
          source,
          paste0(doc_rd, collapse = "")
        )
      )
    }
    i = i + 1
  }

  dots_rd = paste0(dots_rd, collapse = "")
  if (dots_rd != "") {
    topic$add(roxygen2::rd_section("param", c("..." = dots_rd)))
  }
}


get_documented_params = function(topic, only_first = FALSE) {
  documented = names(topic$get_value("param"))
  if (length(documented) > 0) {
    documented = strsplit(documented, ",")
    if (only_first) {
      documented = purrr::map_chr(documented, 1)
    } else {
      documented = unlist(documented)
    }
  }

  documented[documented == "\\dots"] = "..."
  documented
}

find_params = function(name, topics, source) {
  topic = get_rd(name, topics, source = source)
  if (is.null(topic)) {
    return()
  }

  params = topic_params(topic)
  if (is.null(params)) {
    return()
  }

  param_names = stringr::str_trim(names(params))
  param_names[param_names == "\\dots"] = "..."

  Map(
    list,
    name = strsplit(param_names, ",\\s*"),
    value = unlist(params)
  )
}

topic_params = function(x) {
  if (inherits(x, "Rd")) {
    arguments = get_tags(x, "\\arguments")
    if (length(arguments) != 1) {
      return(list())
    }
    items = get_tags(arguments[[1]], "\\item")

    values = sapply(items, function(y) rd2text(y[[2]], attr(x, "package")))
    params = sapply(items, function(y) rd2text(y[[1]], attr(x, "package")))

    stats::setNames(values, params)
  } else {
    x$get_value("param")
  }
}

# RD file processing ----

rd2text = function(x, package) {
  x = tweak_links(x, package)
  # TODO Roxygen2 has a complex way of gettng the text out of an Rd.
  # Not clear to me whether it is needed, and it is thwoing a mysterious error
  # Seen end for implementation of as_character_rd
  chr = unlist(x) # as_character_rd(structure(x, class = "Rd"), deparse = TRUE)
  paste(chr, collapse = "")
}

# Convert relative to absolute links
tweak_links = function(x, package) {
  tag = attr(x, "Rd_tag")

  if (is.list(x)) {
    if (!is.null(tag) && tag == "\\link") {
      opt = attr(x, "Rd_option")
      if (is.null(opt)) {
        if (has_topic(x[[1]], package)) {
          attr(x, "Rd_option") = structure(package, Rd_tag = "TEXT")
        }
      } else if (rlang::is_string(opt) && substr(opt, 1, 1) == "=") {
        topic = substr(opt, 2, nchar(opt))

        if (has_topic(topic, package)) {
          attr(x, "Rd_option") = structure(
            paste0(package, ":", topic),
            Rd_tag = "TEXT"
          )
        }
      }
    } else if (length(x) > 0) {
      x[] = lapply(x, tweak_links, package = package)
    }
  }

  x
}

# Find info in Rd or topic ------------------------------------------------

get_rd = function(name, topics, source) {
  if (is_namespaced(name)) {
    # External package
    parsed = rlang::parse_expr(name)
    pkg = as.character(parsed[[2]])
    fun = as.character(parsed[[3]])

    get_rd_from_help(pkg, fun, source)
  } else {
    # Current package
    topic = topics %>%
      Find(f = function(t) t$get_name() == name)
    if (is.null(topic)) {
      warn_roxy_topic(
        source,
        "@inherits failed to find topic {.str {name}}"
      )
    }
    topic
  }
}

get_rd_from_help = function(package, alias, source) {
  if (!rlang::is_installed(package)) {
    warn_roxy_topic(
      source,
      "@inherits failed because {.pkg {package}} is not installed"
    )
    return()
  }

  help = utils::help((alias), (package))
  if (length(help) == 0) {
    warn_roxy_topic(
      source,
      "@inherits failed to find topic {package}::{alias}"
    )
    return()
  }

  out = internal_f("utils", ".getHelpFile")(help)
  attr(out, "package") = package
  out
}

# Roxygen shims -----

is_namespaced = function(x) {
  tryCatch(
    {
      expr = rlang::parse_expr(x)
      rlang::is_call(expr, "::", n = 2)
    },
    error = function(err) FALSE
  )
}


# Link resolution ----


#' Find the Rd file of a topic
#'
#' @param pkg Package to search in, or `NA` if no package was specified.
#'   If the same as the dev package, then we treat it as `NA`.
#' @param topic Topic to search for. This is the escaped, so it is `"\%\%"` and
#'   not `"%%"`.
#' @param tag The roxy tag object that contains the link. We use this for
#'   better warnings, that include the file name and line number (of the tag).
#' @return String. File name to link to.
#'
#' @details
#' If `pkg` is `NA` or the package being documented, we'll just leave the
#' topic alone.
#'
#' If `pkg` is not `NA` and not the package being documented (the _dev_
#' package), then we need to be able to find the Rd file. If we can't, that's
#' a warning and the link is left untouched. This typically happens when the
#' linked package is not installed or cannot be loaded.
#'
#' @noRd
find_topic_filename = function(pkg, topic, tag = NULL) {
  if (is.na(pkg) || identical(roxygen2::roxy_meta_get("current_package"), pkg)) {
    topic
  } else {
    try_find_topic_in_package(pkg, topic, tag)
  }
}

#' Find a help topic in a package
#'
#' This is used by both `find_topic_filename()` and
#' `format.rd_section_reexport()` that creates the re-exports page. The error
#' messages are different for the two, so errors are not handled here.
#'
#' @param pkg Package name. This cannot be `NA`.
#' @inheritParams find_topic_filename
#' @return File name if the topic was found, `NA` if the package could be
#'   searched, but the topic was not found. Errors if the package cannot be
#'   searched. (Because it is not installed or cannot be loaded, etc.)
#'
#' @noRd
find_topic_in_package = function(pkg, topic) {
  # This is needed because we have the escaped text here, and parse_Rd will
  # un-escape it properly.
  on.exit(close(con), add = TRUE)
  con = textConnection(topic)
  raw_topic = stringr::str_trim(tools::parse_Rd(con)[[1]][1])
  basename(utils::help((raw_topic), (pkg))[1])
}

try_find_topic_in_package = function(pkg, topic, tag) {
  path = tryCatch(
    find_topic_in_package(pkg, topic),
    error = function(err) {
      warn_roxy_topic(
        topic,
        "refers to unavailable topic {pkg}::{topic}",
        parent = err
      )
      topic
    }
  )

  if (is.na(path)) {
    warn_roxy_topic(
      topic,
      "refers to unavailable topic {pkg}::{topic}"
    )
    topic
  } else {
    path
  }
}

resolve_qualified_link = function(topic) {
  if (is_namespaced(topic)) {
    target = stringr::str_split_fixed(topic, "::", n = 2)
    file = find_topic_in_package(target[1], target[2])
    paste0(target[1], ":", file)
  } else {
    paste0("=", topic)
  }
}

internal_f = function(p, f) {
  stopifnot(is.character(p), length(p) == 1)
  stopifnot(is.character(f), length(f) == 1)
  get(f, envir = asNamespace(p))
}

get_tags = function(rd, tag) {
  Filter(function(x) identical(attr(x, "Rd_tag"), tag), rd)
}


has_topic = function(topic, package) {
  tryCatch(
    {
      out = exec("help", topic, package, .env = global_env())
      inherits(out, "dev_topic") ||
        (inherits(out, "help_files_with_topic") && length(out) == 1)
    },
    error = function(c) FALSE
  )
}

warn_roxy = function(file, line, message, parent = NULL, envir = parent.frame()) {
  link = cli::style_hyperlink(
    paste0(basename(file), ":", line),
    paste0("file://", file),
    params = c(line = line, col = 1)
  )

  message[[1]] = paste0(link, ": ", message[[1]], ".")
  names(message)[[1]] = "x"
  cli::cli_inform(message, parent = parent, .envir = envir)
}

warn_roxy_topic = function(topic, message, parent = NULL, envir = parent.frame()) {
  message[[1]] = paste0("In topic '", topic, "': ", message[[1]], ".")
  names(message)[[1]] = "x"
  cli::cli_inform(message, parent = parent, .envir = envir)
}


# TODO: This is here for a reason but throws an error when invoked by rd2text
# Generated in .onLoad() why? not clear
# as_character_rd = NULL
# make_as_character_rd = function() {
#   # "as.character.Rd" appears to a few commands in TWOARGS
#   # this code hacks the body of the function to add it
#   fn = internal_f("tools", "as.character.Rd")
#
#   body = body(fn)
#   idx = purrr::detect_index(body, ~ rlang::is_call(.x, "<-", 2) && rlang::is_symbol(.x[[2]], "TWOARG"))
#   if (idx == 0) {
#     return(fn)
#   }
#
#   body[[idx]][[3]] = rlang::call_modify(body[[idx]][[3]], "\\href", "\\ifelse", "\\if")
#   body(fn) = body
#   fn
# }
