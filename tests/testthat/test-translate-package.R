test_that("translate_package arg checking errors work", {
  expect_error(translate_package(c("dplyr", "data.table")), "Only one package at a time", fixed = TRUE)
  expect_error(translate_package(1), "'dir' must be a character", fixed = TRUE)
  expect_error(translate_package(languages = 1L), "'languages' must be a character vector", fixed = TRUE)
})

test_that("translate_package handles empty packages", {
  pkg <- test_path("test_packages/no_msg")
  restore_package(pkg, {
    expect_invisible(translate_package(pkg))

    expect_message(translate_package(pkg, verbose = TRUE), "No messages to translate", fixed = TRUE)
  })
})
