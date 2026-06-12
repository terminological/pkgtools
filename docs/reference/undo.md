# Undo a bulk `pkgtools` operation

Reverts to last state committed by `pkgtools`, and stashes any unstaged
changes. All `pkgtools` functions that modify files commit before making
changes and record the commit hash. This function rolls back to that
commit.

## Usage

``` r
undo(pkg = ".")
```

## Arguments

- pkg:

  the package

## Value

nothing - called for side effects

## Examples

``` r
if (FALSE) {
  # After making changes with fix_unqualified_fns_bulk():
  undo()
  # To redo the changes: gert::git_stash_pop()
}
```
