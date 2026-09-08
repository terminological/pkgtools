# Implementation Notes: A Roclet for `@unit` Test Generation

Date: 2026-09-08
Status: design notes (not yet implemented)

## Background

`@unit` currently works **without** a roclet by piggybacking on roxygen2's
`rd` roclet:

- `roxy_tag_parse.roxy_tag_unit` — parses the tag body as example code.
- `roxy_tag_rd.roxy_tag_unit` — an S3 method that roxygen2's
  `roclet_process.roclet_rd` calls **while rendering each Rd topic**. The
  method does two things:
  1. returns an `rd_section("unit", ...)` → the "Unit tests" section in the `.Rd`
  2. **side effect**: writes/updates the `tests/testthat/test-unit-*.R`
     file and `unit-test-metadata.csv` (`R/roxygen_unit.R:101-233`)

To know which file/fence to update, (2) has no access to the roxygen *block*,
so it recovers it with `.search_call_stack(.class = "roxy_block")` — the
fragile call-stack convention documented in `design/queries.md` #3.

Now that `pkgtools_rd` exists (`R/roxygen_inheritDotParams.R`), the file
writing can move into proper roclet phases, where the blocks are available
directly. This note describes how.

## Roclet lifecycle facts (verified against roxygen2 8.1.0 source)

From `roxygen2::roxygenise()`:

```r
roclets <- lapply(roclets, roclet_find)                        # name -> roclet object
roclets <- lapply(roclets, roclet_preprocess, blocks, base_path)
results <- lapply(roclets, roclet_process, blocks, env, base_path)  # ALL process first
out     <- map2(roclets, results, roclet_output, base_path = base_path, is_first = ...)
```

1. **Order:** every roclet's `process` runs to completion before any
   roclet's `output` runs.
2. **State channel process → output:** the *same* roclet object is passed to
   both phases, but R is copy-on-write — assigning to `x` inside
   `roclet_process()` does **not** propagate back to the object that
   `roclet_output()` receives. Use
   `roxygen2::roxy_meta_set(key, value)` / `roxy_meta_get(key, default)`
   instead. `roxy_meta` is an in-memory env that is cleared and reloaded from
   DESCRIPTION at the start of each `roxygenise()` run; it is **not** written
   to disk, so nothing leaks between `document()` calls.
   (`roxygenise` itself uses this channel: `local_roxy_meta_set("env", env)`.)
3. **Roclet name resolution:** `roclet_find()` `eval()`s the string from
   `Roxygen: ... roclets = c(...)` in an env whose parent is roxygen2's
   namespace, then (via base) the search path. The Roxygen `packages` field
   triggers `loadNamespace("pkgtools")`, so an unqualified `pkgtools_rd`
   resolves; `pkgtools::pkgtools_rd` (as written by `use_pkgtools()`) is the
   explicit, safer form. S3 methods (`roxy_tag_rd.roxy_tag_unit`,
   `roclet_process.roclet_pkgtools_rd`, ...) are found through S3 method
   registration, so a loaded namespace is sufficient — no attach needed.
4. **Tag methods still run under a subclassed rd roclet:**
   `roclet_process.roclet_rd` builds topics by calling
   `roxy_tag_rd(tag, env = env, base_path = base_path)` for every tag. Since
   `pkgtools_rd` has class `c("roclet_pkgtools_rd", "roclet_rd", "roclet")`,
   calling `NextMethod()` in our `roclet_process` still renders the
   `@unit`/`@dparam` Rd sections.
5. **No default methods exist** for `roclet_process`, `roclet_output`, or
   `roclet_clean` (only per-class: `rd`, `namespace`, `vignette`). Any class
   you introduce yourself must define its own methods, or inherit `"rd"`.
   (`roclet_preprocess` has a default that returns `x` unchanged.)
6. **Object shapes:**
   - block: `roxy_block` = `list(tags, file, line, call, object)`;
     `object$topic` is the function name for function docs, `NULL` for
     `@name`-only blocks.
   - tag: `roxy_tag` = `list(file, line, raw, tag, val)`; for `@unit`,
     `val` is the parsed code string (from `roxy_tag_parse.roxy_tag_unit`),
     `raw` the raw text, `line` the line in the source file.

## Interaction with `@noRd` (fixes the current limitation)

Current approach: `block_to_rd()` calls `needs_doc(block)`, which returns
`FALSE` for any block carrying `@noRd`, and the function returns **before**
the per-tag `roxy_tag_rd()` loop runs. So `roxy_tag_rd.roxy_tag_unit` never
fires for `@noRd` blocks and no test file is generated.

**Neither option is affected.** Both collect specs by iterating over the
parsed `blocks` handed to `roclet_process()`, not over the processed
topics, and:

- `parse_package()` includes `@noRd` blocks — noRd filtering happens only
  inside `block_to_rd()` (the Rd-rendering path).
- Verified empirically (roxygen2 8.1.0, fixture with `@noRd` + `@unit`):
  the block appears in `blocks` with its `unit` tag intact **and**
  `object$topic` set — object resolution happens at parse time, independent
  of noRd. So the `.topic_of_block()` fallback chain works unchanged for
  noRd blocks.

Result: under either option, `@noRd` + `@unit` functions get test files
generated. The only thing that remains noRd-dependent is the "Unit tests"
`.Rd` section — `@noRd` means no `.Rd` file exists, so there is nowhere to
put it; that is the intended semantics.

One rule for Option A: the collection code in
`roclet_process.roclet_pkgtools_rd` must iterate over `blocks` (as sketched)
and must **not** derive specs from the `topics` returned by `NextMethod()`
— that would silently reintroduce the noRd gap.

## Option A (recommended): extend the existing `pkgtools_rd` roclet

One roclet does both jobs. Data flows process → output via `roxy_meta`.

### 1. Collect `@unit` specs during process

```r
#' @importFrom roxygen2 roclet_process
#' @export
roclet_process.roclet_pkgtools_rd = function(x, blocks, env, base_path) {
  # Existing: standard Rd processing + inheritDotParams post-processing
  topics = NextMethod()
  topics = .postprocess_inherit_dot_params(topics, blocks, env)  # (current inline code)

  # NEW: collect @unit specs for the output phase
  specs = unlist(lapply(blocks, function(block) {
    tags = Filter(~ .x$tag == "unit", block$tags)
    purrr::map(tags, function(tag) .unit_spec(block, tag))
  }), recursive = FALSE)
  roxygen2::roxy_meta_set("pkgtools.unit_specs", specs)

  topics
}

.unit_spec = function(block, tag) {
  list(
    topic   = .topic_of_block(block),
    is_fn   = !is.null(block$object) && !is.null(block$object$topic),
    file    = block$file,          # e.g. "/abs/path/R/foo.R"
    relpath = stringr::str_extract(block$file, "^(.*?/)(R/.*)$", 2),
    line    = tag$line,
    code    = tag$raw
  )
}

# Same fallback chain as the current roxy_tag_rd.roxy_tag_unit, but using
# block fields directly — no .search_call_stack needed.
.topic_of_block = function(block) {
  if (!is.null(block$object) && !is.null(block$object$topic)) {
    return(block$object$topic)
  }
  name_tags = Filter(~ .x$tag == "name", block$tags)
  if (length(name_tags) > 0) {
    return(name_tags[[1]]$val)
  }
  deparse(block$call[[2]])
}
```

### 2. Write test files during output

```r
#' @importFrom roxygen2 roclet_output
#' @export
roclet_output.roclet_pkgtools_rd = function(x, results, base_path, ...) {
  paths = NextMethod()                       # writes .Rd files

  specs = roxygen2::roxy_meta_get("pkgtools.unit_specs", default = list())
  if (length(specs) > 0) {
    env = roxygen2::roxy_meta_get("env")
    .write_unit_tests(specs, env = env, base_path = base_path)
  }
  paths
}
```

### 3. `.write_unit_tests()` — extract the file-writing branch

Move the body of the `if (write) { ... }` block (plus the skeleton/metadata
setup) out of `roxy_tag_rd.roxy_tag_unit` into a standalone function, one
loop iteration per spec:

```r
.write_unit_tests = function(specs, env, base_path) {
  pkg = unname(getNamespaceName(env))
  test_meta = fs::path(base_path, "tests/testthat/unit-test-metadata.csv")
  metadata = .read_metadata(test_meta)       # current read/create logic

  for (spec in specs) {
    test_path = gsub("/R/", "/tests/testthat/test-unit-", spec$file)

    # current write-decision: skip if metadata$updated > source mtime
    last_updated = suppressWarnings(max(
      metadata %>%
        dplyr::filter(topic == spec$topic,
                      file == fs::path_rel(test_path, start = fs::path_home())) %>%
        dplyr::pull(updated)
    ))
    if (fs::file_exists(test_path) && length(last_updated) > 0 &&
        last_updated > fs::file_info(spec$file)$modification_time) {
      next
    }

    if (!fs::file_exists(test_path)) {
      fs::dir_create(fs::path_dir(test_path))
      readr::write_lines(.create_skeleton(unit_test_skeleton, pkg = pkg,
                                          .insert_before = "# end of unit tests ----"),
                         test_path)
    }

    test_lines = readr::read_lines(test_path)

    # port unchanged: dead-fence deletion (env check), expect-no-error
    # wrapper, style_text, .update_fenced_block, metadata bind_rows,
    # cli::cli_inform("Updated unit tests for `%s`.", spec$topic)
    ...
    readr::write_lines(test_lines, test_path)
    metadata = .append_metadata(metadata, test_path, spec$topic)
  }
  readr::write_csv(metadata, test_meta)
}
```

The fence helpers (`.update_fenced_block`, `.delete_fenced_block`,
`.glue_recover_pieces`, `.glue_detect_output`, `.create_skeleton`) and the
whisker templates (`unit_test_call`, `unit_test_skeleton`,
`no_test_code_wrapper`) move over verbatim; they are already pure.

### 4. Shrink `roxy_tag_rd.roxy_tag_unit` to pure rendering

```r
roxy_tag_rd.roxy_tag_unit = function(x, base_path, env) {
  roxygen2::rd_section("unit", list(code = x$raw))
}
```

This removes the last `@unit` use of `.search_call_stack` (closes
`design/queries.md` #3 for this path) and makes the tag method side-effect
free, as roxygen2 intends. `roxy_tag_parse.roxy_tag_unit` is unchanged.

## Option B (alternative): separate `pkgtools_unit` roclet

If you want the concerns decoupled (unit-test generation must work even if
the Rd roclet is customised/replaced):

```r
pkgtools_unit = function() {
  roxygen2::roclet("pkgtools_unit")      # NOTE: no "rd" inheritance
}

#' @export
roclet_process.roclet_pkgtools_unit = function(x, blocks, env, base_path) {
  # Return the specs directly — a roclet's own process result is passed
  # verbatim to its output via map2(), so no roxy_meta channel is needed.
  unlist(lapply(blocks, function(block) {
    tags = Filter(~ .x$tag == "unit", block$tags)
    purrr::map(tags, function(tag) .unit_spec(block, tag))
  }), recursive = FALSE)
}

#' @export
roclet_output.roclet_pkgtools_unit = function(x, results, base_path, ...) {
  if (length(results) > 0) {
    env = roxygen2::roxy_meta_get("env")
    .write_unit_tests(results, env = env, base_path = base_path)
  }
  invisible(NULL)
}

#' @export
roclet_clean.roclet_pkgtools_unit = function(x, base_path) {
  dir(fs::path(base_path, "tests/testthat"), pattern = "^test-unit-.*\\.R$") %>%
    fs::file_delete()
  fs::file_delete(fs::path(base_path, "tests/testthat/unit-test-metadata.csv"))
}
```

Registration: `Roxygen: list(..., roclets = c("collate", "pkgtools_rd",
"pkgtools_unit", "namespace"))`, and `use_pkgtools()` must add both roclet
names. `roxy_tag_rd.roxy_tag_unit` is still required (pure version, Option A
step 4) for the "Unit tests" `.Rd` section — the unit roclet does not touch
Rd output.

**Trade-offs.** B passes data through `results` (cleaner than the meta
channel) and adds a `roclet_clean` for `clean = TRUE` runs, but costs an
extra registered roclet and a second pass over the blocks. A keeps one
roclet and reuses the object already in DESCRIPTION; its only wart is the
`roxy_meta` hand-off. Either way the file-writing code is the same
`.write_unit_tests()`.

## Migration checklist

1. Extract `.write_unit_tests()`, `.unit_spec()`, `.topic_of_block()`
   (Option A steps 1–3; or B + shared helpers).
2. Delete the file-writing branch from `roxy_tag_rd.roxy_tag_unit`; keep the
   `rd_section` return (pure version).
3. (Option B only) add `pkgtools_unit` + its three methods; extend
   `use_pkgtools()` to register it; add to pkgtools' own DESCRIPTION.
4. `devtools::document()` on pkgtools itself (dogfood) and diff
   `tests/testthat/test-unit-*.R` — generated output must be byte-identical.
   If it is not, fix the generator, not the generated files.
5. Remove the now-dead `test_path_rel` code from `roxy_tag_rd.roxy_tag_unit`
   and any `@unit` tags that only existed to test the old write path.

## Pitfalls

- **Do not mutate the roclet object** in `roclet_process()` expecting
  `roclet_output()` to see it — copy-on-write makes that a no-op. Only
  `roxy_meta_*` (Option A) or the process return value (Option B) carry data.
- **`roxy_meta` is per-run.** It is cleared at the start of every
  `roxygenise()`. Never read a key that was only set in a previous run;
  always set it in `process` of the same run before `output`.
- **`output` runs on every `document()`**, even when no Rd changed. The
  current mtime-vs-metadata skip decision is what keeps this idempotent —
  keep it (or switch to render-and-compare, which additionally overwrites
  hand-edits of `test-unit-*.R`; that is a behaviour change, decide
  deliberately).
- **`NextMethod()` depends on class order.** Keep `"rd"` as a class of
  `pkgtools_rd` and keep the method named `roclet_process.roclet_pkgtools_rd`.
- **A bare custom class needs all three methods** (`process`, `output`,
  `clean`) — there are no defaults. For Option A, `clean` dispatches to
  `roclet_clean.roclet_rd` (deletes roxygen `.Rd` files) but nothing cleans
  `test-unit-*.R`; add `roclet_clean.roclet_pkgtools_rd` if you want
  `clean = TRUE` to regenerate tests.
- **Roclet names in DESCRIPTION** are `eval()`ed with roxygen2's namespace as
  parent — unqualified names only resolve because Roxygen `packages` triggers
  `loadNamespace`. Keep `packages = "pkgtools"` or use `pkgtools::pkgtools_unit`.
- **`env` in process/output is the loaded package namespace** (the
  `load_code` strategy), which the current code uses for
  `getNamespaceName(env)` and the dead-function `eval()` check — preserve
  those usages, do not substitute the global env.
- **File writes during `process` are the anti-pattern being removed.** Other
  roclets' processing is in flight at that point; the only reason the current
  code writes there is that it had no roclet.

## Test plan

Manual test (new `tests/testthat/test-unit-roclet.R`, following the tempdir
fixture pattern in `test-use_unit_test.R`):

1. Build a fixture package in `tempdir()`: minimal DESCRIPTION with
   `Roxygen: list(markdown = TRUE, packages = "pkgtools", roclets = c("pkgtools_rd"))`
   (or via `use_pkgtools(fix_dir)`), plus `R/foo.R` with two functions, one
   carrying an `@unit` tag.
2. Run `roxygen2::roxygenise(fix_dir)` and assert:
   - `tests/testthat/test-unit-foo.R` exists with the expected fence
     (`# unit test start: <topic> ----`) and styled code;
   - `man/*.Rd` contains the "Unit tests" section;
   - `unit-test-metadata.csv` has a row with a home-relative path.
3. Re-run `roxygenise()` unchanged: test file mtime unchanged (skip logic).
4. Delete the function, re-run: fence block is removed.
5. Snapshot/hash tests (`@unit` tags) for the pure helpers:
   `.topic_of_block()` (function / `@name` / neither cases),
   `.unit_spec()`, and `.write_unit_tests()` against a scratch dir.

Definition of done: `devtools::document()` on pkgtools itself produces zero
diff in `tests/testthat/test-unit-*.R`, and the full test suite passes.
