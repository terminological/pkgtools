# Regex find and replace on a codebase

Find and replace within a package after committing the current contents

## Usage

``` r
find_and_replace(
  regex,
  replacement,
  pkg = ".",
  rDirectories = c("data-raw", "R", "vignettes", "tests")
)
```

## Arguments

- regex:

  a PCRE regex

- replacement:

  a replacement

- pkg:

  the package to scan

- rDirectories:

  the subdirectories (defaults to data-raw, R, vignettes, tests)

## Value

a list of changed files

## Examples

``` r
if (FALSE) {
  # Replace all instances of an old function name:
  find_and_replace("old_fn", "new_fn")
}
```
