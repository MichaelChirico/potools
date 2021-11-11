test_that("user is told what's happening", {
  temp <- local_test_package("R/test.r" = "message('Hello')")
  withr::local_dir(temp)

  po_extract()
  po_create(c("ja", "fr"))

  expect_normalized_snapshot(po_update(verbose = TRUE, lazy = FALSE))
})
