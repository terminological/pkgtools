# Changelog

## pkgtools 0.3.0

- Added comprehensive unit test suite (211 tests) covering exported and
  internal functions.
- Added `@design/queries.md` documenting design expectation
  vs. implementation gaps.
- Improved roxygen documentation across all R source files with typos
  fixes, `@concept` tags, `@return` corrections, `@examples`, and
  `@noRd` annotations for internal functions.
- Added vignette article documenting RStudio addins and keyboard
  shortcut bindings: `rstudio-addins.Rmd`
  - Execute commented code (`Ctrl+Enter` by convention)
  - Hash-based snapshot test case generation (`Ctrl+P`)
  - Equality-based test case generation (`Ctrl+O`)
- Added vignette: `getting-started.Rmd` — overview and decision guide
  for pkgtools workflows.
- Added vignette: `standalone-files.Rmd` — standalone file management
  deep-dive.
- Added vignette: `bulk-fixes.Rmd` — bulk linting and fixes deep-dive.
- Grouped all 4 vignettes under a “Guides” section in `_pkgdown.yml`.
- Updated `_pkgdown.yml` with new article navigation and reference
  grouping by `@concept` tags.
- Added hex sticker: `inst/hexsticker/hex-sticker.png` (600×600,
  “Subtle” design).
- Moved templates from `data_documentation.R` to `R/templates-data.R` to
  avoid roxygen2 parsing issues with whisker template strings.
- Added `.env` and `updated` to
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  to suppress “no visible binding” notes.
- Added `.Rprofile` with `jquerylib` patch for
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  when `libjs-jquery` is not installed on the system.
- Added `.Rbuildignore` entries for sandbox artifacts and non-standard
  files.
- Moved `knitr` and `ggplot2` from Imports to Suggests (used only in
  vignettes and examples).
