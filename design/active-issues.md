# Active Issues

Pre-existing issues discovered during review. Not fixed here per AGENTS.md;
suggested fixes documented.

---

## 1. `tests/testthat/unit-test-metadata.csv` stores absolute paths

**Status:** pre-existing (committed rows use `/home/scoder/...`; uncommitted
working-tree rows use `/home/vp22681/...`).

The CSV records absolute test-file paths, so the same logical rows are
duplicated across machines/home directories. This makes the file
environment-dependent and breaks dedup by path.

**Suggested fix:** store package-relative paths (e.g.
`tests/testthat/test-unit-import-standalone-codegen.R`) and deduplicate
existing rows. Regenerate/refresh logic in the codegen that writes this file
(`R/import-standalone-codegen.R`) accordingly.

---

## 2. `.escape_utf8()` logic is fragile (see `design/queries.md` #10)

**Status:** pre-existing; flagged as Medium severity in `design/queries.md`.

The escape → mark → unescape → unmark chain may cancel itself out for some
inputs (e.g. `.escape_utf8("café")` may return the input unchanged).

**Suggested fix:** add a regression test in
`tests/testthat/test-utils.R` covering non-ASCII edge cases (Latin-1,
superscripts, emoji, already-escaped sequences) and verify the round-trip
behaviour before relying on it in `fix_utf8_encoding`.

---

## 3. WIP: `use_unit_test` → `use_pkgtools` rename leaves NAMESPACE stale

**Status:** work-in-progress in the working tree, not pre-existing, but
documented here so it isn't mistaken for a bug.

`R/use_unit.R` now defines `use_pkgtools()` while `NAMESPACE` still exports
`use_unit_test`, and `tests/testthat/test-use_unit_test.R` still calls
`pkgtools::use_unit_test()` (will fail). Completing the work requires:

1. `devtools::document()` to regenerate NAMESPACE + man/
2. Rename `test-use_unit_test.R` to `test-use_pkgtools.R` and update the call
3. Decide whether `use_unit_test` should remain as a deprecated wrapper

---

## 4. WIP: `R/roxygen_inheritDotParams.R` is new and untracked

**Status:** work-in-progress; the `pkgtools_rd` roclet (3-level transitive
`@inheritDotParams`) plus the new `man/pkgtools_rd.Rd` are not yet committed.

Note: it muffles all roxygen2 messages matching `inheritDotParams`
(`roclet_process.roclet_pkgtools_rd`), which will also hide *genuine* failures
(e.g. a typo in the source function name) rather than only suppressing the
"no arguments inherited" noise. Consider narrowing the pattern to the exact
"failed." message.
