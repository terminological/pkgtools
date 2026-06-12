# Detect glue outputs

This allows you to filter for lines that match those that could have
been generated with a given glue spec

## Usage

``` r
.glue_detect_output(glue_spec, content)
```

## Arguments

- glue_spec:

  a glue spec with braces determining named substitutes

- content:

  a character vector

## Value

a logical vector with true if the character could have been created by
the glue spec.

## Unit tests


    content = c("sdlfksl","  # X, Y and Z ----","sdfskjfsdlk asdlkfjsdlkj")
    testthat::expect_equal(
      .glue_detect_output("# {name}, {value1} and {value2} ----", content),
      c(FALSE,TRUE,FALSE)
    )
