# metadata ----------------------------------------------------------------

test_that("can find R and src translations", {
  temp <- local_test_package()
  file.create(file.path(temp, "po", c("R-en.po", "en.po")))

  meta <- withr::with_dir(temp, get_po_metadata())
  expect_equal(meta$language, c("en", "en"))
  expect_setequal(meta$type, c("R", "src"))
})

test_that("get_po_metadata() returns 0 rows if no .po fles", {
  temp <- local_test_package()
  meta <- get_po_metadata(temp)
  expect_equal(nrow(meta), 0)
})

test_that("po_compile() can handle UTF-8 msgstr", { 
  temp <- local_test_package(
    `R/foo.R` = "foo <- function() message('Hello!')"
  )

  po_extract(temp) # R/* -> .pot
  po_create("es", temp) # .pot -> R-es.po
  r_es_po <- file.path(temp, "po", "R-es.po")
  l <- readLines(r_es_po)
  l[grep('msgstr ""', l)[2L]] <- 'msgstr "\U00A1Hello!"'
  cat(l, file = r_es_po, sep = "\n")

  expect_no_error(po_compile(temp, verbose=FALSE))
})
