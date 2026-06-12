# Use a template file

Expands a whisker-based template file by substituting named
placeholders. Templates are stored in `inst/templates/` and selected
from via a menu if `.template_name` is not specified. The resulting file
is written to `R/generated-*.R`.

## Usage

``` r
use_template(.template_name = NULL, ..., .overwrite = FALSE, .destfile = NULL)
```

## Arguments

- .template_name:

  if not given a dialog will be presented with the options

- ...:

  supplied as parameters to the template. template placeholders such as
  `` `xxx.#name#_yyy` ``, will be substituted with `xxx.myname_yyy` if
  `name="myname"`. The first value of this list will be used for
  `` `##` `` placeholders

- .overwrite:

  overwrte the destination file if it exists

- .destfile:

  if given will write the expanded template here.

## Value

the written file name

## Examples

``` r
# List available templates:
# list.files(system.file("templates", package = "pkgtools"))

# Use a template by name:
# use_template("some_template", name = "mything", .destfile = "R/generated-mything.R")
```
