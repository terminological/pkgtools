# Compare content of editor with last saved version

I frequently end up getting an unsaved change in RStudio conflicting
with an update on disk. Often as a result of a global find and replace.
This helps determine which version to keep, by bringing up a 2 way diff
of editor content and disk content. The result of the merge is placed
back in the editor.

## Usage

``` r
what_has_changed()
```

## Value

nothing - called for side effects

## Examples

``` r
if (FALSE) {
  # From within RStudio, open a file with unsaved changes:
  what_has_changed()
}
```
