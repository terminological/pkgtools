# Deletes content between markers in an input

This function deletes anything between matching markers in an input
vector including the markers themselves. This will match multiple
markers if present and paired

## Usage

``` r
.delete_fenced_block(
  .input,
  name = "block",
  ...,
  .start_glue = "# {name} start ----",
  .end_glue = "# {name} end ----"
)
```

## Arguments

- .input:

  the input as a character vector

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

## Value

a character vector with the deleted blocks

## Unit tests



    tmp = .delete_fenced_block(letters,.start_glue = letters[4], .end_glue = letters[10])
    testthat::expect_true(!any(letters[4:10] 

    # multiple copies of fenced block are removed
    # fenced block to end of file is removed
    testthat::expect_equal(
      .delete_fenced_block(rep(letters,3),.start_glue = "b", .end_glue = "z"),
      rep("a",3)
    )

    # fenced block at beginning is removed:
    testthat::expect_equal(
      .delete_fenced_block(letters,.start_glue = "a", .end_glue = "y"),
      "z"
    )

    # fenced block
    testthat::expect_equal(
      .delete_fenced_block(letters,.start_glue = "a", .end_glue = "z"),
      character()
    )

    testthat::expect_equal(
      .delete_fenced_block(letters,.start_glue = 2, .end_glue = -2),
      c("a","z")
    )
