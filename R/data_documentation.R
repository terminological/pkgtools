#' Auto generate data documentation
#'
#' @param data an arbitrary R object
#'
#' @returns the data description block formatted in markdown
#' @export
#'
#' @examples
#' generate_data_description(
#'   iris,
#'   title = "The myiris dataset",
#'   description = c("line 1 & test","line 2","","para 2", paste(rep("asda ",50),collapse="")),
#'   url = "http://example.com",
#'   name = "myiris"
#' )
#'
#' nested = ggplot2::diamonds %>% tidyr::nest(details = -c(cut,color,clarity))
#'
#' generate_data_description(
#'   nested,
#'   title = "The nested diamonds dataset",
#'   description = "The ggplot2 diamonds data set nested",
#'   url = "http://example.com",
#'   name = "mydiamonds"
#' )
generate_data_description = function(
  data,
  title = sprintf("The `%s` dataset", name),
  description = sprintf("`%s` dataset description", name),
  url = "http://example.com",
  name = NULL
) {
  if (is.null(name)) {
    name = deparse(substitute(data))
  }

  description = paste0(description, collapse = "\n")
  description2 = strsplit(description, "\n\n")[[1]]
  description3 = lapply(description2, strwrap, 75)
  description3[-1] = lapply(description3[-1], function(x) c("", x))
  description3 = unlist(description3)

  tmp = NULL
  if (is.data.frame(data)) {
    tmp = .render_df(data, name)
  } else if (is.list(data) && all(nzchar(names(data)))) {
    tmp = .render_list(data, name)
  } else {
    tmp = .object_type(data)
  }

  out = whisker::whisker.render(
    .data_description,
    list(
      name = name,
      title = title,
      description = description3,
      details = as.list(tmp),
      url = url
    )
  )

  return(strsplit(out, "\n")[[1]])
}

.object_type = function(x) {
  if (inherits(x, "data.frame")) {
    return("dataframe")
  }
  return(pillar::type_sum(x))
}


.listish_structure = function(nms, object, nosummary) {
  structure = lapply(nms, function(nm) {
    col = object[[nm]]
    needs_entry = NULL
    subtype_doc = NULL
    if (is.data.frame(col)) {
      # must have come from a list
      nm2 = sprintf("df[%s]", nm)
      subtype = nm2
      subtype_doc = .render_df(col, nm2, nosummary = nosummary)
    } else if (is.list(col)) {
      nonempty = Filter(Negate(is.null), col)
      if (length(nonempty) > 1) {
        itemtype = .object_type(nonempty[[1]])
        if (itemtype == "dataframe") {
          nm2 = sprintf("df[%s]", nm)
          subtype = sprintf("list[%s]", nm2)
          subtype_doc = .render_df(nonempty[[1]], nm2, nosummary = nosummary)
        } else if (itemtype == "named list") {
          nm2 = sprintf("list[%s]", nm)
          subtype = sprintf("list[%s]", nm2)
          subtype_doc = .render_list(nonempty[[1]], nm2, nosummary = nosummary)
        } else {
          subtype = sprintf("list[%s]", itemtype)
        }
      } else {
        subtype = "list[NULL]"
      }
    } else {
      subtype = .object_type(col)
    }
    return(list(
      name = nm,
      type = subtype,
      nosummary = nosummary,
      has_subtype = !is.null(subtype_doc),
      subtype = subtype_doc
    ))
  })
  return(structure)
}

.render_list = function(named_list, name, nosummary = FALSE) {
  structure = .listish_structure(names(named_list), named_list, nosummary)
  summary = sprintf("named list with %d items", length(named_list))

  out = whisker::whisker.render(
    .df_description("item"),
    list(
      name = name,
      nosummary = nosummary,
      summary = summary,
      structure = structure,
      subtypes = Filter(function(s) s$has_subtype, structure)
    )
  )

  return(strsplit(out, "\n")[[1]])
}

.render_df = function(df, name, nosummary = FALSE) {
  structure = .listish_structure(colnames(df), df, nosummary = TRUE)
  summary = sprintf("dataframe with %d rows and %d columns", nrow(df), ncol(df))

  out = whisker::whisker.render(
    .df_description("column"),
    list(
      name = name,
      nosummary = nosummary,
      summary = summary,
      structure = structure,
      subtypes = Filter(function(s) s$has_subtype, structure)
    )
  )

  return(strsplit(out, "\n")[[1]])
}

## Templates ----

.data_description = "
## documentation block for `{{name}}` ----

#' {{title}}
#' 
{{#description}}
#' {{&.}}
{{/description}}
#'
{{#details}}
#' {{&.}}
{{/details}}
#'
#' @usage data(\"{{name}}\")
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
{{#url}}
#' @source <{{.}}>
{{/url}}
\"{{name}}\"

## end of documentation block for `{{name}}`
"

.df_description = function(item_label) {
  tmp = "
{{^nosummary}}
## `{{name}}` {{summary}}

{{/nosummary}}
\\describe{
{{#structure}}
    \\item{ `{{name}}` ({{type}}{{#has_subtype}}*{{/has_subtype}}) }{ 
        {{name}} description 
      }
{{/structure}} }

{{#subtypes}}
{{#nosummary}}
### `{{name}}` %s (`{{type}}` type)
{{/nosummary}}
{{#subtype}}
{{.}}
{{/subtype}}
{{/subtypes}}
"
  return(sprintf(tmp, item_label))
}
