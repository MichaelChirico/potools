# metadata ----------------------------------------------------------------

test_that("can find R and src translations", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", c("R-en.po", "en.po")))

  meta <- withr::with_dir(temp, get_po_metadata())
  expect_equal(meta$language, c("en", "en"))
  expect_setequal(meta$type, c("R", "src"))
})

test_that("get_po_metadata() returns 0 rows if no .po fles", {
  meta <- get_po_metadata(temp)
  expect_equal(nrow(meta), 0)
})
