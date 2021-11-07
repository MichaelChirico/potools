# metadata ----------------------------------------------------------------

test_that("can find R and src translations", {
  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "po"))
  file.create(file.path(temp, "po", c("R-en.po", "en.po")))

  meta <- withr::with_dir(temp, get_po_metadata(package = "test"))
  expect_equal(meta$language, c("en", "en"))
  expect_setequal(meta$type, c("R", "src"))
})
