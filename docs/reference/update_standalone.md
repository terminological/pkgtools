# Update or create a standalone metadata block in the Rstudio editor.

This function helps identify package imports and maintains standalone
file metadata. It operates on the code in the Rstudio active window and
suggests changes to be made to keep the standalone file metadata up to
date. It is intended be used interactively.

## Usage

``` r
update_standalone(repo = NULL, license = NULL, dependencies = NULL)
```

## Arguments

- repo:

  the github repository e.g. `organisation/project`

- license:

  a license URL (defaults to `https://unlicense.org`)

- dependencies:

  a optional list of standalone filenames in the same repository

## Value

nothing, updates the RStudio editor content

## Examples

``` r
if (FALSE) {
  # From RStudio with a standalone file open:
  update_standalone("terminological/ggrrr")
}
```
