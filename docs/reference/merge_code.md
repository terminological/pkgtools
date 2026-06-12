# Merge Code Versions using Shiny and CodeMirror 5 MergeView

This function launches a Shiny gadget to visually merge two versions of
code/YAML.

## Usage

``` r
merge_code(
  value,
  new,
  old = NULL,
  lhs = "original",
  rhs = "suggested",
  accept = "Save",
  rhs_accept = "Accept all",
  mode = "r"
)
```

## Arguments

- value:

  the content for the central panel.

- new:

  character vector of code lines. Content of the changed / RHS version.

- old:

  character vector of code lines. Content of the old / LHS version.

- lhs:

  the title of the LHS

- rhs:

  the title of the RHS

- accept:

  the button text for the LHS

- rhs_accept:

  the button text for the RHS

- mode:

  the language mode

## Value

Character string of the merged content, or stops with an error if
cancelled.

## Examples

``` r
if (FALSE) {

  old = paste0("line ",c(2:4,6:12))
  new = paste0("line ",c(1:50))

  merged_result <- merge_code(old, new)
  cat("Merged Content:\n")
  cat(paste0(merged_result, collapse = "\n"),"\n")

}
```
