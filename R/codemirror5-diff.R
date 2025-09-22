#' Merge Code Versions using Shiny and CodeMirror 5 MergeView
#'
#' This function launches a Shiny gadget to visually merge two versions of code/YAML.
#'
#' @param new character vector of code lines. Content of the changed / RHS version.
#' @param old character vector of code lines. Content of the old / LHS version.
#' @param value the content for the central panel.
#' @param lhs the title of the LHS
#' @param rhs the title of the RHS
#' @param accept the button text for the LHS
#' @param rhs_accept the button text for the RHS
#' @param mode the language mode
#'
#' @return Character string of the merged content, or stops with an error if cancelled.
#'
#' @concept edit
#'
#' @export
#' @examples
#'
#' if (FALSE) {
#'
#'   old = paste0("line ",c(2:4,6:12))
#'   new = paste0("line ",c(1:50))
#'
#'   merged_result <- merge_code(old, new)
#'   cat("Merged Content:\n")
#'   cat(paste0(merged_result, collapse = "\n"),"\n")
#'
#' }
#'
merge_code <- function(
  value,
  new,
  old = NULL,
  lhs = "original",
  rhs = "suggested",
  accept = "Save",
  rhs_accept = "Accept all",
  mode = "r"
) {
  content_a = if (!is.null(old)) paste0(old, collapse = "\n") else NULL
  content_b = if (!is.null(new)) paste0(new, collapse = "\n") else NULL
  content_value = paste0(value, collapse = "\n")

  if (
    (is.null(content_b) || content_value == content_b) &&
      (is.null(content_a) || content_value == content_a)
  ) {
    message("No difference to compare.")
    return(unlist(strsplit(content_value, "\n")))
  }

  inclJs = function(script) {
    shiny::includeScript(system.file(
      paste0("www/", script),
      package = "pkgtools"
    ))
  }

  inclCss = function(css) {
    shiny::includeCSS(system.file(paste0("www/", css), package = "pkgtools"))
  }

  # --- UI Definition ---
  ui <- shiny::fillPage(
    inclJs("codemirror-5/lib/codemirror.js"),
    inclJs("codemirror-5/mode/r/r.js"),
    inclJs("codemirror-5/mode/markdown/markdown.js"),
    inclJs("codemirror-5/mode/yaml/yaml.js"),
    inclJs("google/diff_match_patch.js"),
    inclJs("codemirror-5/addon/merge/merge.js"),
    inclJs("codemirror5-diff.js"),
    inclCss("codemirror-5/lib/codemirror.css"),
    inclCss("codemirror-5/addon/merge/merge.css"),
    inclCss("codemirror5-diff.css"),

    bslib::layout_column_wrap(
      shiny::h3(lhs, style = "text-align: center"),
      shiny::h3("", style = "text-align: center"),
      shiny::h3(rhs, style = "text-align: center"),
      col_widths = 1 / 3
    ),
    shiny::tags$div(
      id = "merge-view-container",
      style = "border: 1px solid #ccc; height: 80vh; width: 100%; overflow: auto; scroll-behavior: smooth;"
    ),
    shiny::br(),
    bslib::layout_column_wrap(
      shiny::actionButton("cancel", "Cancel", class = "btn-block"),
      shiny::actionButton(
        "save",
        accept,
        class = "btn-primary btn-block"
      ),
      shiny::actionButton(
        "saveAll",
        rhs_accept,
        class = "btn-primary btn-block"
      ),
      col_widths = 1 / 3
    )
  ) # End of UI

  # --- Server Logic ---
  server <- function(input, output, session) {
    # Send initialization data to JavaScript after the client is ready
    session$onFlushed(function() {
      session$sendCustomMessage(
        "initialize_mergeview",
        list(
          origLeft = content_a, # Version A (left pane) thi maybe null
          origRight = content_b, # Version B (right pane) - MergeView might use 'other' instead
          value = content_value,
          mode = mode # Pass mode for potential language handling in JS
        )
      )
    })

    # Handle Save button click
    shiny::observeEvent(input$save, {
      session$sendCustomMessage("request_merged_content", list())
    })

    # Listen for the merged content sent back from JavaScript
    shiny::observeEvent(input$merged_content, {
      shiny::stopApp(input$merged_content)
    })

    # Handle Save button click
    shiny::observeEvent(input$saveAll, {
      shiny::stopApp(content_b)
    })

    # Handle Cancel button click
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(if (is.null(content_a)) content_value else content_a)
    })

    # Listen for potential errors from JS
    shiny::observeEvent(input$merged_content_error, {
      # Send cleanup message to JS
      shiny::stopApp(stop(
        paste0("Merge failed in browser: ", input$merged_content_error),
        call. = FALSE
      ))
    })
  } # End of Server

  # --- Run the Gadget ---
  out = shiny::runGadget(
    app = shiny::shinyApp(ui, server),
    # viewer = shiny::dialogViewer("Merge View", width = 1200, height = 800)
    viewer = shiny::paneViewer()
  )
  return(unlist(strsplit(unlist(out), "\n")))
}
