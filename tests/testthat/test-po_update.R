test_that("user is told what's happening", {
  temp <- local_test_package("R/test.r" = "message('Hello')")
  withr::local_dir(temp)

  po_extract()
  po_create(c("ja", "fr"))

  # verbose output for shell commands on Windows uses " in shQuote (vs ' on unix)
  expect_snapshot(
    po_update(verbose = TRUE, lazy = FALSE),
    transform = standardise_dots,
    variant = .Platform$OS.type
  )
})

test_that("user is told what's happening", {
  temp <- local_test_package("R/test.r" = "message('Hello')")
  withr::local_dir(temp)

  po_extract()
  po_create("fr")

  writeLines("message('Hi')", file.path(temp, "R/test.R"))
  po_extract()
  po_update()

  expect_false(file.exists(file.path(temp, "po/R-fr.po~")))
})

test_that("lazy=TRUE, verbose=TRUE skips up-to-date files and messages", {
  temp <- local_test_package("R/test.r" = "message('Hello')")
  withr::local_dir(temp)

  po_extract()
  po_create("ja")

  # run once to make sure it's "up-to-date"
  po_update()

  # Ensure mtime is different.
  Sys.sleep(1.25)

  expect_silent(
    po_update(lazy = TRUE, verbose = FALSE)
  )
  expect_output(
    po_update(lazy = TRUE, verbose = TRUE),
    "Skipping 'ja' R translation (up-to-date)",
    fixed = TRUE
  )
})
