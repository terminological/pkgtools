# Delete backup files from the current project

This is largely deprecated by the move to git commits for backup.
Deletes all `.old` files recursively.

## Usage

``` r
delete_backups(pkg = ".")
```

## Arguments

- pkg:

  the package to delete files from.

## Value

nothing - called for side effects

## Examples

``` r
if (FALSE) {
  delete_backups()
}
```
