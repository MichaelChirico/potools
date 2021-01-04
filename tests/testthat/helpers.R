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

expect_messages = function(expr, msgs, ...) {
  observed_messages = capture_messages(expr)
  expect_true(all(vapply(msgs, function(msg) any(grepl(msg, observed_messages, ...)), logical(1L))))
}
