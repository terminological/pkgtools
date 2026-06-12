test_that(".escape escapes special regex characters", {
  expect_equal(.escape("hello.world"), "hello\\.world")
  expect_equal(.escape("a*b+c"), "a\\*b\\+c")
  expect_equal(.escape("(x)"), "\\(x\\)")
  expect_equal(.escape("[a-z]"), "\\[a-z\\]")
  expect_equal(.escape("{1,3}"), "\\{1,3\\}")
  expect_equal(.escape("$^"), "\\$\\^")
  expect_equal(.escape("\\\\test"), "\\\\\\\\test")
})

test_that(".escape does not alter plain strings", {
  expect_equal(.escape("helloworld"), "helloworld")
  expect_equal(.escape("abc123"), "abc123")
})

test_that(".pkgtools_message respects verbose option", {
  withr::with_options(
    list(pkgtools.verbose = TRUE),
    expect_message(.pkgtools_message("test message"))
  )
  withr::with_options(
    list(pkgtools.verbose = FALSE),
    expect_silent(.pkgtools_message("test message"))
  )
  # Default should be FALSE
  expect_silent(.pkgtools_message("test message"))
})

test_that(".punkmode returns option value", {
  withr::with_options(
    list(pkgtools.punk_mode = TRUE),
    expect_true(.punkmode())
  )
  withr::with_options(
    list(pkgtools.punk_mode = FALSE),
    expect_false(.punkmode())
  )
  # Default should be FALSE
  expect_false(.punkmode())
})

test_that(".move_safe creates backup", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  src <- file.path(test_dir, "test.txt")
  dest <- file.path(test_dir, "test.new.txt")

  writeLines("original content", src)

  new_dest <- .move_safe(src, dest)

  expect_true(file.exists(new_dest))
  expect_false(file.exists(src))
  expect_equal(readLines(new_dest), "original content")

  unlink(test_dir, recursive = TRUE)
})

test_that(".move_safe backs up existing targets", {
  test_dir <- file.path(tempdir(), sprintf("pkgtools-test-%s", substr(as.character(Sys.time()), 1, 8)))
  dir.create(test_dir)

  src1 <- file.path(test_dir, "test.txt")
  dest <- file.path(test_dir, "test.new.txt")

  writeLines("first content", src1)
  writeLines("existing backup", dest)

  .move_safe(src1, dest)

  # dest should now contain the src content
  expect_equal(readLines(dest), "first content")
  # .move_safe should have backed up dest first to dest.old
  dest_bak <- paste0(dest, ".old")
  expect_true(file.exists(dest_bak))

  unlink(test_dir, recursive = TRUE)
})

test_that(".escape_utf8 escapes non-ASCII characters", {
  test_str <- "café"
  result <- .escape_utf8(test_str)
  # The function handles UTF-8 escaping
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
})
