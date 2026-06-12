# Fix unqualified functions in active source pane

Interactively find and replace unqualified, e.g. `mutate(...)` calls
with fully qualified `dplyr::mutate(...)` calls. This version operates
on the currently open file in the RStudio and allows for interactive
review of the proposed changes.

## Usage

``` r
fix_unqualified_fns()
```

## Value

nothing - called for side effects

## Examples

``` r
if (FALSE) {
  # From within RStudio with a file open:
  fix_unqualified_fns()
}
```
