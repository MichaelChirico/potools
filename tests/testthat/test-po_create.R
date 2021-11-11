test_that("the user is told what's happening", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", "R-test.pot"))

  withr::local_dir(temp)
  expect_snapshot(po_create("jp", verbose = TRUE))
  expect_snapshot(po_create("jp", verbose = TRUE))
})

test_that("can generate both R and src pot files", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", c("R-test.pot", "test.pot")))

  expect_equal(pot_types(temp), c("R", "src"))

  files <- withr::with_dir(temp, po_language_files("en"))
  expect_equal(files$type, c("R", "src"))
  expect_equal(files$po_path, file.path(".", "po", c("R-en.po", "en.po")))
  expect_equal(files$pot_path, file.path(".", "po", c("R-test.pot", "test.pot")))
})

test_that("can create multiple languages", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", c("R-test.pot", "test.pot")))

  files <- withr::with_dir(temp, po_language_files(c("en", "jp", "ar")))
  expect_equal(nrow(files), 6)
})
