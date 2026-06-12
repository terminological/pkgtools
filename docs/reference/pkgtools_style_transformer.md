# A code transformer for use with [`styler::style_text()`](https://styler.r-lib.org/reference/style_text.html)

This transformer is adapted from `https://github.com/gadenbuie/grkstyle`
It differs in that it will break apart long lines regardless of their
initial line breaks to try and fit them into a set width. It does not
unbreak lines.

## Usage

``` r
pkgtools_style_transformer(..., width = 70)
```

## Arguments

- width:

  the max width of the code (excludes indent hence approximate)

## Unit tests


    # style_text('testthat::expect_equal(iris[1, ], structure(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2, Species = structure(1L, levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1L, class = "data.frame"))')
    # style_text('testthat::expect_equal(iris[1, ], structure(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2, Species = structure(1L, levels = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1L, class = "data.frame"))', width=80)
