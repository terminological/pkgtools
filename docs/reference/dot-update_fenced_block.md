# Update a fenced block within a file with new content based on a template

This function looks for a fenced section (typically of code) with
markers defined by `.start_glue` and `.end_glue` and replaces them with
the evaluated template content. If no matching sections are found the
new code is inserted before the matched `.insert_before` marker (or at
the end if that is not found)

## Usage

``` r
.update_fenced_block(
  .input,
  .template,
  name = "block",
  ...,
  .start_glue = "# {name} start ----",
  .end_glue = "# {name} end ----",
  .insert_before = "# insert before ----",
  .parse_fn = ~list()
)
```

## Arguments

- .input:

  the input as a character vector

- .template:

  the whisker template as a character vector

- name:

  a name for the block

- ...:

  additional named parameters supplied to the whisker template

- .start_glue:

  a glue specification for the marker of the start of the block to be
  replaced. May use any of the whisker variables e.g.
  `"# {name} start ----"`. This can also be given as a numeric index,
  were particularly 0 is the beginning of the input and -1 is the end.
  If numeric it is start inclusive.

- .end_glue:

  a glue specification for the marker of the end of the block to be
  replaced. May use any of the whisker variables e.g.
  `"# {name} end ----"` or numeric as above. If numeric it is end
  inclusive.

- .insert_before:

  a marker for where in the file to insert a block if no matching start
  and end markers are found.

- .parse_fn:

  a function that takes the matching block in the source and extracts a
  named list of data from it. The default returns an empty list.

## Value

A character vector of the updated content of the file. Does not write
the file

## Unit tests


    # double braces in mustache files, single braces in glue specs.
    template = "# {{name}} {{status}}"

    input = c(
      "before",
      "#TEST test start ----",
      "# test original",
      "#TEST test end ----",
      "#TEST insert before ----",
      "end"
    )

    input = .update_fenced_block(
      input, template, name = "test", status = "update",
      .start_glue = "#TEST {name} start ----",
      .end_glue = "#TEST {name} end ----",
      .insert_before = "#TEST insert before ----",
    )

    testthat::expect_equal(input[3], "# test update")

    input = .update_fenced_block(
      input, template, name = "test2", status = "update",
      .start_glue = "#TEST {name} start ----",
      .end_glue = "#TEST {name} end ----",
      .insert_before = "#TEST insert before ----",
    )

    testthat::expect_equal(input[6], "# test2 update")

    .update_fenced_block(
      input, template, name = "test", status = "update",
      .start_glue = 2,
      .end_glue = "#TEST {name} end ----",
    )
