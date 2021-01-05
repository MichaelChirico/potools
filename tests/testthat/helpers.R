# copy a package to tmp, then overwrite any changes on exit
restore_package <- function(dir, expr, tmp_conn) {
  # this is way uglier than it should be. i'm missing something.
  tdir <- tempdir()

  file.copy(dir, tdir, recursive = TRUE)
  on.exit({
    unlink(dir, recursive = TRUE)
    dir.create(dir)
    file.copy(file.path(tdir, basename(dir)), dirname(dir), recursive = TRUE)
    unlink(file.path(tdir, basename(dir)), recursive = TRUE)
  })

  if (!missing(tmp_conn)) {
    old = options("__potools_testing_prompt_connection__" = tmp_conn)
    on.exit(options(old), add = TRUE)
  }

  invisible(capture.output(expr))
}

expect_all_match = function(inputs, target, ..., invert=FALSE) {
  expect_true(all(vapply(inputs, function(input) length(grep(input, target, ..., invert=invert)) > 0L, logical(1L))))
}

expect_messages = function(expr, msgs, ..., invert=FALSE) {
  observed_messages = capture_messages(expr)
  expect_all_match(msgs, observed_messages, ..., invert=invert)
}

expect_outputs = function(output, outs, ..., invert=FALSE) {
  expect_all_match(outs, output, ..., invert=invert)
}

test_package = function(pkg) test_path(file.path("test_packages", pkg))
mock_translation = function(mocks) test_path(file.path("mock_translations", mocks))
