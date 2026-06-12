# Create a skeleton based on a template

Create a skeleton based on a template

## Usage

``` r
.create_skeleton(
  .template = NULL,
  ...,
  .insert_before = "# insert before ----"
)
```

## Arguments

- .template:

  the whisker template as a character vector

- ...:

  additional named parameters supplied to the whisker template

- .insert_before:

  a marker for where in the file to insert a block if no matching start
  and end markers are found.

## Value

A character vector of the file skeleton. does not write the file
