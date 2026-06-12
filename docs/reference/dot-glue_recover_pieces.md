# Extract data from the output of a glue call

This function allow for the reverse of the
[`glue::glue_data()`](https://glue.tidyverse.org/reference/glue.html)
operation and regenerates the data as a dataframe. It won't work for
very complex glue specifications and will only return strings.

## Usage

``` r
.glue_recover_pieces(glue_spec, output)
```

## Arguments

- glue_spec:

  a glue spec with braces determining named substitutes

- output:

  the glue output

## Value

a dataframe with a column per piece which could have been used in
`glue::glue_data(.x,glue_spec)` apart from leading white space.

## Unit tests


    glue_spec = "# {name}, {value1} and {value2} ----"
    output = rep("  # one, two and three ----",5)
    tmp = .glue_recover_pieces(glue_spec,output)

    testthat::expect_equal(nrow(tmp), 5)
    testthat::expect_equal(trimws(output), glue::glue_data(tmp,glue_spec))
