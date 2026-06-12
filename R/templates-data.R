# Whisker templates for data documentation
# These templates generate Rd content - do NOT add roxygen comments to this file.

.data_description_template <- "
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
#' @docType data
#' @keywords datasets
#' @concept datasets
{{#url}}
#' @source <{{.}}>
{{/url}}
\"{{name}}\"

## end of documentation block for `{{name}}`
"
