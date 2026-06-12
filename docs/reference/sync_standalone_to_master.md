# Synchronise master version of standalone file with editor contents

This file syncs the current content of the rstudio editor window with a
local master standalone file, which is assumed to be in a Git directory.
This allows for changes to be propagated back to the master when in
active development. The master copy is committed before changes.

## Usage

``` r
sync_standalone_to_master(git_dir = fs::path_home("Git"))
```

## Arguments

- git_dir:

  absolute or relative path to git base directory (e.g. ~/Git)

## Value

nothing used for side effects

## Examples

``` r
if (FALSE) {
  # Sync the current editor's standalone file to its master copy:
  sync_standalone_to_master()
}
```
