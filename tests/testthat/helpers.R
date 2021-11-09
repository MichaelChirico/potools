# copy a package to tmp, deleting on exit
with_package <- function(dir, expr, msg_conn = NULL) {
  tdir <- withr::local_tempdir()
  file.copy(dir, tdir, recursive = TRUE)
  withr::local_dir(file.path(tdir, basename(dir)))

  if (!is.null(msg_conn)) {
    withr::local_options("__potools_testing_prompt_connection__" = msg_conn)
  }

  mockery::stub(translate_package, "get_atime", function(...) "0000-01-01 00:00:00")
  expr
}

with_restoration_test_that <- function(desc, pkg, code, conn = NULL) {
  pkg <- test_package(pkg)
  if (!is.null(conn)) conn <- mock_translation(conn)
  with_package(pkg, code, conn)
}

# TODO: I think this can just be replaced by expect_match and expect_no_match in current testthat dev
expect_all_match = function(inputs, targets, ..., invert=FALSE) {
  matched <- vapply(
    targets,
    function(target) length(grep(target, inputs, ..., invert=invert)) > 0L,
    logical(1L)
  )

  expect(
    all(matched),
    sprintf(
      if (invert) {
        "Unwanted messages found:\n  Observed: %s\n  Didn't want: %s\n"
      } else {
        "Not all messages found:\n  Observed: %s\n  Wanted: %s\n"
      },
      toString(sQuote(inputs)),
      toString(sQuote(targets[!matched]))
    )
  )
}

expect_messages = function(expr, msgs, ..., invert=FALSE) {
  observed_messages = capture_messages(expr)
  expect_all_match(observed_messages, msgs, ..., invert=invert)
}

test_package = function(pkg) test_path(file.path("test_packages", pkg))
mock_translation = function(mocks) normalizePath(test_path(file.path("mock_translations", mocks)))
