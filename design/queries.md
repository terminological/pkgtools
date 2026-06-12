# Design Queries & Expectation Gaps

This file documents areas where my understanding of the package's behaviour diverged
from the actual implementation during development and testing. Each entry explains:
what I expected, what actually happens, and a question for clarification.

These are primarily observations surfaced during writing unit tests for exported
and internal functions.

---

## 1. `find_and_replace` scans `tests/` by default

**Expected:** `rDirectories` would cover `R/`, `data-raw/`, and `vignettes/` — but not
`tests/`. Test files contain assertions that shouldn't be subject to bulk find-and-replace.

**Actual:** The default is `c("data-raw", "R", "vignettes", "tests")`. It will modify
test files, and when called from inside a package (during `devtools::test`), it even
scans the pkgtools test files themselves.

**Question:** Is including `tests` intentional? If so, the documentation should warn
users that test code will be modified. If unintentional, it should be removed from
the default.

**Code:** `R/utils-find-replace.R:59-62`

---

## 2. `set_renv_repos` requires *named* arguments

**Expected:** The function accepts repository URLs positionally, like `repos()`:

```r
set_renv_repos("terminological", "https://terminological.r-universe.dev", .wd = test_dir)
```

**Actual:** Positional arguments are silently ignored. The function uses
`rlang::list2(...)` internally, which only captures named arguments.

**Fix:** Must be:
```r
set_renv_repos(terminological = "https://terminological.r-universe.dev", .wd = test_dir)
```

**Question:** Should this use `rlang::list2(..., .named = TRUE)` to produce a clear error
on positional use, or should the docs be much more explicit?

**Code:** `R/set_renv_repos.R:15`

---

## 3. `.search_call_stack` requires explicit data flow through all call levels

**Expected:** Calling `.search_call_stack(.class = "data.frame")` from within nested
function calls would walk up the stack and find the first argument that is a data frame.

**Actual:** The function inspects the first parameter *name* of each stack frame and
looks it up in that frame. If a parameter is passed but not forwarded (e.g. `g()` called
from `f(x)` without passing `x`), the lookup finds nothing. The intermediate functions
must explicitly receive and forward the argument:

```r
f <- function(x) { g(x) }  # must pass x through
g <- function(x) { h(x) }  # must pass x through
h <- function(x) {
  df <- .search_call_stack(.class = "data.frame")
  nrow(df)
}
```

**Question:** This is clever but fragile — it depends on the caller's parameter naming.
Should there be a fallback mode that matches by position or by class? Or should this
be documented as a "convention required" function?

**Code:** `R/import-standalone-stack-tools.R:90-118`

---

## 4. `.get_fn_name` returns the binding name, not `"<unknown>"`

**Expected:** Anonymous functions created with `function(x) x` would return
`"<unknown>"` since they have no explicit name.

**Actual:** The function looks up the function object in its calling environment and
returns the variable name it's bound to:

```r
anon <- function(x) x
.get_fn_name(anon)  # returns "anon", NOT "<unknown>"
```

Only a truly unbound anonymous function (e.g. passed directly as an argument) returns
`"<unknown>"`. This is actually more useful than the original expectation.

**Question:** No action needed — this is better behaviour. But should it be documented
explicitly since the `@unit` examples suggest it returns `<unknown>` for anonymous
functions?

**Code:** `R/import-standalone-stack-tools.R:45-59`

---

## 5. `standalone_snapshot` returns a multi-line character vector

**Expected:** A single-line string suitable for copy-paste into test code.

**Actual:** Returns a 4-element character vector (one element per line), because
`style_text()` wraps long expressions. To get valid R code you need
`paste(result, collapse = "\n")`.

```r
standalone_snapshot(42, .clip = FALSE)
# [1] "testthat::expect_equal("  "  rlang::hash(my_obj),"   "\"hashvalue\""            ")"
```

**Question:** Should the function accept a `multiline = FALSE` option for convenience,
or is the multi-line output intentional (for readability)?

**Code:** `R/roxygen_addins.R:140-167`

---

## 6. `style_text` converts `<-` to `=` unconditionally

**Expected:** Code like `x <- 1` would be formatted but preserve `<-`.

**Actual:** The pkgtools transformer forces `=` assignment:

```r
style_text("x <- 1")  # returns "x = 1"
```

This uses styler's `force_assignment_op` transformer and is a deliberate style
opinion, but it's a significant departure from typical R conventions.

**Question:** Should this be documented prominently as "pkgtools style prefers `=` over
`<-`"? And should there be an option to toggle it?

**Code:** `R/pkgtools-styler.R:64-70`

---

## 7. `.update_fenced_block` replaces content entirely

**Expected:** The function would merge or append new content alongside existing
content between fence markers.

**Actual:** The template completely replaces everything between the start and end
markers. Previous content is discarded:

```r
input <- c("#TEST start", "# old", "#TEST end")
.update_fenced_block(input, "# {{status}}", name = "test", status = "new", ...)
# Result: "#TEST start", "# test new", "#TEST end"  (old is gone)
```

**Question:** Is there value in a "merge" mode that preserves existing content?
The current name "update" might mislead users into thinking the old content
survives.

**Code:** `R/import-standalone-codegen.R:73-162`

---

## 8. `doc_formals` returns `"(default \\code{`NULL`} [NULL])"` for missing parameters

**Expected:** Calling `doc_formals(fn, nonexistent_param)` would return `""`
(empty string), allowing callers to distinguish "no parameter" from "has a default."

**Actual:** Returns `"(default \\code{`NULL`} [NULL])"` — the evaluated default of
`NULL` when the parameter doesn't exist.

```r
doc_formals(test_fn, missing_param)  # "(default \\code{`NULL`} [NULL])"
```

**Question:** Should truly missing parameters return `""` and parameters with
`NULL` as default return a distinct indicator like `"(no default)"`?

**Code:** `R/doc_formals.R:30-55`

---

## 9. `use_unit_test` emits `cli_alert_info()` messages

**Expected:** A setup function called once could run silently.

**Actual:** Always emits:
```
i Registering pktools build-time dependency in DESCRIPTION
i Registering pkgtools usage in DESCRIPTION
```

(Note: **"pktools"** is a typo in the second message.)

**Question:** Two issues:
1. `expect_silent()` is the wrong assertion for a user-facing function — the messages
   are user feedback. Should I use `expect_message()` instead?
2. There is a typo: `"pktools"` should be `"pkgtools"` on the second alert line.

**Code:** `R/use_unit.R:44-48`

---

## 10. `.escape_utf8` does not always produce `\uXXXX` output

**Expected:** Non-ASCII characters like `café` would be converted to
`caf\u00e9`.

**Actual:** The function's three-step process (escape all Unicode → mark UTF-8 with
markers → unescape → replace markers back) may cancel itself out for certain inputs:

```r
.escape_utf8("café")  # may return the original string unchanged
```

The logic is:
1. `stringi::stri_escape_unicode()` — escapes all Unicode to `\uXXXX`
2. `str_replace_all` — wraps each `\uXXXX` with `@UTF...\UTF@` markers
3. `stringi::stri_unescape_unicode()` — unescapes them back to original bytes
4. `str_replace_all` — replaces markers with `\uXXXX`

But step 3 may unescape what step 1 produced, and step 4's regex may not match.

**Question:** Does this work for all non-ASCII strings, or is there a bug with
certain characters? This logic chain is fragile and would benefit from
a regression test covering edge cases.

**Code:** `R/utils.R:83-90`

---

## Summary

| # | Function | Severity | Action |
|---|----------|----------|--------|
| 1 | `find_and_replace` | Low | Docs or default adjustment |
| 2 | `set_renv_repos` | Medium | Better error or docs |
| 3 | `.search_call_stack` | Low | Docs (convention required) |
| 4 | `.get_fn_name` | None | Better docs (feature, not bug) |
| 5 | `standalone_snapshot` | Low | Optional convenience param |
| 6 | `style_text` | Low | Prominent docs |
| 7 | `.update_fenced_block` | Low | Docs or merge mode |
| 8 | `doc_formals` | Low | Distinct indicators |
| 9 | `use_unit_test` | **Medium** | Fix typo "pktools" |
| 10 | `.escape_utf8` | **Medium** | Regression test needed |
