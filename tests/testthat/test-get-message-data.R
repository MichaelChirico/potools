test_that("Packages with src code & C syntax errors fail gracefully", {
  expect_error(
    get_message_data(test_package("r_src_err_1")),
    "Parsing error: found an odd number (3)", fixed = TRUE
  )
  expect_error(
    get_message_data(test_package("r_src_err_2")),
    "Parsing error: unmatched parentheses", fixed = TRUE
  )
  expect_error(
    get_message_data(test_package("r_src_err_3")),
    "Parsing error: unmatched parentheses", fixed = TRUE
  )
})

test_that("Partially named messaging arguments are an error", {
  expect_error(
    get_message_data(test_package("plural_semi_named")),
    "found a call to ngettext that names only some of its messaging arguments",
    fixed = TRUE
  )
})
