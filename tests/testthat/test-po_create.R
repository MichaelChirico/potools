test_that("multiplication works", {
  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "po"))
  file.create(file.path(temp, "po", "R-test.pot"))
  writeLines("Package: test", file.path(temp, "DESCRIPTION"))

  withr::local_dir(temp)
  expect_snapshot(po_create("jp", verbose = TRUE))
  expect_snapshot(po_create("jp", verbose = TRUE))
})
