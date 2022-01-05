test_that("user is told what's happening", {
  temp <- local_test_package("R/test.r" = "message('Hello')")
  withr::local_dir(temp)

  po_extract()
  po_create(c("ja", "fr"))

  expect_normalized_snapshot(po_update(verbose = TRUE, lazy = FALSE))
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
