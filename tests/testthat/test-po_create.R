test_that("multiplication works", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", "R-test.pot"))

  withr::local_dir(temp)
  expect_snapshot(po_create("jp", verbose = TRUE))
  expect_snapshot(po_create("jp", verbose = TRUE))
})
