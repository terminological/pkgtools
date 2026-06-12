# Style code in `pkgtools` style

This is adapted from `https://github.com/gadenbuie/grkstyle`. It differs
in that it will break apart long lines regardless of their initial line
breaks to try and fit them into a set width. It does not unbreak lines.

## Usage

``` r
style_text(text, ..., transformers = pkgtools_style_transformer(...))
```

## Arguments

- text:

  A character vector with text to style.

- ...:

  Arguments passed on to
  [`styler::style_text`](https://styler.r-lib.org/reference/style_text.html),
  [`pkgtools_style_transformer`](https://terminological.github.io/pkgtools/reference/pkgtools_style_transformer.md)

  `style`

  :   A function that creates a style guide to use, by default
      [`tidyverse_style`](https://styler.r-lib.org/reference/tidyverse_style.html).
      Not used further except to construct the argument `transformers`.
      See
      [`style_guides()`](https://styler.r-lib.org/reference/style_guides.html)
      for details.

  `include_roxygen_examples`

  :   Whether or not to style code in roxygen examples.

  `base_indention`

  :   Integer scalar indicating by how many spaces the whole output text
      should be indented. Note that this is not the same as splitting by
      line and add a `base_indention` spaces before the code in the case
      multi-line strings are present. See 'Examples'.

  `width`

  :   the max width of the code (excludes indent hence approximate)

- transformers:

  A set of transformer functions. This argument is most conveniently
  constructed via the `style` argument and `...`. See 'Examples'.

## Value

the formatted code

## Examples

``` r
if (FALSE) {
  style_text("my_function(very_long_argument_name = some_value)")
}
```
