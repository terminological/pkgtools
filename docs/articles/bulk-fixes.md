# Bulk Fixes for Package Development

Running `R CMD check` on a package often reveals the same categories of
issues: unqualified function calls, missing namespace imports,
undeclared global variables, non-ASCII characters, and non-standard
files. `pkgtools` provides automated fixes for these common problems.

This vignette explains the fix functions, how they work, and best
practices for using them in your development workflow.

## The Automated Fix Pipeline

[`pkgtools::fix_check()`](https://terminological.github.io/pkgtools/reference/fix_check.md)
is a convenience function that runs all fixes in sequence:

``` r
pkgtools::fix_check()
```

This calls, in order:

1.  [`fix_unqualified_fns_bulk()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns_bulk.md)
    — add namespace prefixes
2.  [`qcheck()`](https://terminological.github.io/pkgtools/reference/qcheck.md)
    — run `R CMD check` (without examples/tests)
3.  [`fix_non_standard_files()`](https://terminological.github.io/pkgtools/reference/fix_non_standard_files.md)
    — update `.Rbuildignore`
4.  [`fix_global_variables()`](https://terminological.github.io/pkgtools/reference/fix_global_variables.md)
    — update `globals.R`
5.  [`fix_utf8_encoding()`](https://terminological.github.io/pkgtools/reference/fix_utf8_encoding.md)
    — escape non-ASCII characters
6.  [`fix_dependencies()`](https://terminological.github.io/pkgtools/reference/fix_dependencies.md)
    — fix Imports/Suggests in `DESCRIPTION`

Each step uses the output of the check to identify and fix issues.

## Safety First: Git Backups

All bulk fix functions follow the same safety pattern:

1.  **Commit** any pending changes before modifying files
2.  **Record** the commit hash in an internal undo stack
3.  **Ask** for confirmation before making changes (in interactive mode)

This means you can always undo a bulk fix operation.

``` r
# Roll back to the state before the last pkgtools operation:
pkgtools::undo()

# To redo the changes:
gert::git_stash_pop()
```

## Fix Unqualified Function Names

### The Problem

R package development requires fully qualified function calls when
referencing functions from imported packages. Without the prefix,
`R CMD check` reports “no visible binding for global variable” warnings.

``` r
# Problem: unqualified
filter(data, x > 1)
mutate(data, y = x * 2)

# Fix: qualified
dplyr::filter(data, x > 1)
dplyr::mutate(data, y = x * 2)
```

### The Fix

``` r
pkgtools::fix_unqualified_fns_bulk(
  pkg = ".",
  rDirectories = c(here::here("R"), here::here("tests/testthat")),
  prioritise = c("dplyr", "rlang", "stringr", "forcats", "ggplot2",
                 "purrr", "tidyr", "readr", "stats", "utils")
)
```

This function:

1.  **Scans** files in the specified directories for unqualified calls
2.  **Resolves** which package each function comes from (using
    `package_map()` which checks imports, loaded packages, and the
    current package)
3.  **Applies** the namespace prefix via regex
4.  **Reports** which files were modified

### Handling Priorities

When a function name exists in multiple packages (e.g.,
[`lag()`](https://rdrr.io/r/stats/lag.html) is in both `stats` and
`dplyr`), the `prioritise` parameter controls which package is used.
Functions are matched against packages in this order:

1.  **Prioritised packages** (specified in the argument)
2.  **Current package**
3.  **Base and utils** (never qualified)
4.  **Other imports** in the order declared in `DESCRIPTION`

### Interactive vs Bulk Mode

The interactive version,
[`fix_unqualified_fns()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns.md),
operates on the currently open file in RStudio and uses the
[`merge_code()`](https://terminological.github.io/pkgtools/reference/merge_code.md)
gadget for visual review of changes. The bulk version modifies files
directly (after confirmation).

## Fixing Dependencies

### The Problem

`R CMD check` reports:

- “imports not declared from: `package`” — a package is used but not
  listed in `Imports` or `Suggests`
- “not imported from: `package`” — a package is listed but not used

### The Fix

``` r
# Run with automatic check:
pkgtools::fix_dependencies()

# Or with existing check output:
check <- qcheck()
pkgtools::fix_dependencies(check = check)
```

This function:

1.  **Parses** the check output for import-related messages
2.  **Extracts** the package names from warning/note text
3.  **Removes** unnecessary packages from `DESCRIPTION`
4.  **Adds** missing packages to `Imports`
5.  **Writes** the updated `DESCRIPTION` file

### Unicode in Check Output

The function handles the curly quotes (`` `package` ``) that appear in
`R CMD check` output by using Unicode-aware regex patterns.

## Fixing Global Variables

### The Problem

`R CMD check` reports:

    for global variable 'myvar'

This means a variable is used in package code but not declared in a
`globals.R` file or via
[`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html).

### The Fix

``` r
pkgtools::fix_global_variables()
```

This function:

1.  **Parses** the check output for “global variable” messages
2.  **Reads** the existing `R/globals.R` file (or creates one)
3.  **Adds** missing variable names to the
    [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
    call
4.  **Writes** the updated `globals.R` file

Example `globals.R`:

``` r
utils::globalVariables(c(
  "myvar",
  "other_var",
  "data$column"
))
```

The function handles both simple variable names and `$`-referenced
columns.

## Fixing Non-Standard Files

### The Problem

`R CMD check` reports:

    found at top level: `mydir`
    hidden files and directories: `.svn`

These are files or directories that `R CMD build` would try to include
in the package but shouldn’t be.

### The Fix

``` r
pkgtools::fix_non_standard_files()
```

This function:

1.  **Parses** the check output for “found at top level” and “hidden
    files” messages
2.  **Escapes** special characters in file names for `.Rbuildignore`
3.  **Adds** entries to `.Rbuildignore`
4.  **Preserves** existing entries (no duplicates)

Example `.Rbuildignore`:

    ^mydir$
    ^\.svn$
    ^\.git$

## Fixing UTF-8 Encoding

### The Problem

`R CMD check` reports:

    non-ASCII characters: 'file.R'

This means the file contains characters outside the ASCII range, which
can cause portability issues.

### The Fix

``` r
pkgtools::fix_utf8_encoding()
```

This function:

1.  **Parses** the check output for “non-ASCII characters” messages
2.  **Escapes** non-ASCII characters in the affected files using
    `\uXXXX` Unicode escapes
3.  **Writes** the modified files

The escaping uses a three-step process:

1.  Escape all Unicode with
    [`stringi::stri_escape_unicode()`](https://rdrr.io/pkg/stringi/man/stri_escape_unicode.html)
2.  Mark UTF-8 characters with unique delimiters
3.  Unescape and replace markers with `\uXXXX` sequences

This preserves the file’s encoding while making it ASCII-compatible.

## Interactive Fixing

### Per-File Fixing

For individual files, use the interactive version:

``` r
pkgtools::fix_unqualified_fns()
```

This function:

1.  **Reads** the current file from the RStudio source editor
2.  **Proposes** changes (adding namespace prefixes)
3.  **Launches** the
    [`merge_code()`](https://terminological.github.io/pkgtools/reference/merge_code.md)
    gadget for visual review
4.  **Applies** the merged result back to the editor

### Merge Code Gadget

The
[`merge_code()`](https://terminological.github.io/pkgtools/reference/merge_code.md)
function provides an interactive three-panel diff viewer:

- **Left panel**: the original code
- **Right panel**: the suggested changes
- **Middle panel**: the merged result

Buttons: - **Save**: accept the current middle-panel content - **Accept
all**: accept the right-panel content - **Cancel**: revert to the
original

This is used by several pkgtools functions: -
[`fix_unqualified_fns()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns.md) -
[`merge_code()`](https://terminological.github.io/pkgtools/reference/merge_code.md)
(directly) -
[`what_has_changed()`](https://terminological.github.io/pkgtools/reference/what_has_changed.md) -
[`use_template()`](https://terminological.github.io/pkgtools/reference/use_template.md)
(when overwriting existing files)

### Customising the Gadget

``` r
merged <- pkgtools::merge_code(
  old = "original code",
  new = "suggested code",
  old = "original code",
  value = "current content",
  lhs = "Original",
  rhs = "Suggested",
  accept = "Save",
  rhs_accept = "Accept all",
  mode = "r"  # or "markdown", "yaml"
)
```

## Quick Check Without Running Code

[`qcheck()`](https://terminological.github.io/pkgtools/reference/qcheck.md)
runs `R CMD check` with `--no-examples --no-tests --ignore-vignettes`,
which catches structural issues without executing code:

``` r
check <- pkgtools::qcheck()
```

This is useful for:

- **Fast feedback** during development (no test execution)
- **CI pipelines** where tests are run separately
- **Debugging** import/namespace issues before running tests

The function also optionally runs:

- `spelling::spell_check_package()` (if spelling is installed)
- [`urlchecker::url_check()`](https://rdrr.io/pkg/urlchecker/man/url_check.html)
  (if urlchecker is installed)

## Workflow Example

A typical fix workflow might look like:

``` r
# 1. Make some changes to your package
# 2. Run automated fixes
pkgtools::fix_check()

# 3. If something went wrong, undo
pkgtools::undo()
gert::git_stash_pop()

# 4. Or continue with targeted fixes
pkgtools::fix_unqualified_fns_bulk()
pkgtools::fix_dependencies()
pkgtools::fix_global_variables()

# 5. Verify with qcheck
pkgtools::qcheck()

# 6. If all good, commit and push
gert::git_add(".")
gert::git_commit("fix package linting issues")
```

## Comparing with `devtools::document()`

[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
handles roxygen2-based documentation but does **not** fix code-level
issues. `pkgtools`’s fix functions are complementary:

| Tool                                                                                                                      | Fixes | Documentation  |
|---------------------------------------------------------------------------------------------------------------------------|-------|----------------|
| [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)                                              | No    | Yes (Rd files) |
| [`pkgtools::fix_check()`](https://terminological.github.io/pkgtools/reference/fix_check.md)                               | Yes   | No             |
| [`pkgtools::fix_unqualified_fns_bulk()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns_bulk.md) | Yes   | No             |

Use both in your workflow:

``` r
pkgtools::fix_check()    # fix code issues
devtools::document()     # regenerate documentation
```

## Common Pitfalls

### Multiple Packages with Same Function Name

If [`lag()`](https://rdrr.io/r/stats/lag.html) exists in both `stats`
and `dplyr`, the `prioritise` parameter controls which is used. Adjust
this list for your project’s dependencies:

``` r
pkgtools::fix_unqualified_fns_bulk(
  prioritise = c("dplyr", "tidyr", "purrr", "stats", "utils")
)
```

### Comments and Strings

The unqualified function finder uses regex, not parsing, so it may
occasionally match function names in comments or strings. Review the
proposed changes carefully.

### Non-ASCII in Different Encodings

The UTF-8 fixer assumes files are encoded as UTF-8. If your files use a
different encoding, the escaping may not produce correct results.
Convert files to UTF-8 first.

### Globals in S3 Methods

[`fix_global_variables()`](https://terminological.github.io/pkgtools/reference/fix_global_variables.md)
adds variables to `globals.R`, but S3 method dispatch uses different
rules. Check the R CMD check output manually if you have S3 methods with
undeclared variables.

## Related Functions

| Function                                                                                                        | Purpose                           |
|-----------------------------------------------------------------------------------------------------------------|-----------------------------------|
| [`fix_check()`](https://terminological.github.io/pkgtools/reference/fix_check.md)                               | Run all fixes in sequence         |
| [`fix_unqualified_fns_bulk()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns_bulk.md) | Add namespace prefixes            |
| [`fix_unqualified_fns()`](https://terminological.github.io/pkgtools/reference/fix_unqualified_fns.md)           | Interactive per-file fixing       |
| [`fix_dependencies()`](https://terminological.github.io/pkgtools/reference/fix_dependencies.md)                 | Fix Imports/Suggests              |
| [`fix_global_variables()`](https://terminological.github.io/pkgtools/reference/fix_global_variables.md)         | Update globals.R                  |
| [`fix_utf8_encoding()`](https://terminological.github.io/pkgtools/reference/fix_utf8_encoding.md)               | Escape non-ASCII characters       |
| [`fix_non_standard_files()`](https://terminological.github.io/pkgtools/reference/fix_non_standard_files.md)     | Update .Rbuildignore              |
| [`qcheck()`](https://terminological.github.io/pkgtools/reference/qcheck.md)                                     | Quick check without running code  |
| [`undo()`](https://terminological.github.io/pkgtools/reference/undo.md)                                         | Roll back last pkgtools operation |

## Funding

The authors gratefully acknowledge the support of the UK Research and
Innovation AI programme of the Engineering and Physical Sciences
Research Council [EPSRC grant
EP/Y028392/1](https://gtr.ukri.org/projects?ref=EP%2FY028392%2F1).
