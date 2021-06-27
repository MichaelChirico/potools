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

test_that("Custom translation functions work in R and src", {
  message_data = get_message_data(
    test_package("custom_translation"),
    custom_translation_functions = list(
      R = c('MyTranslator:fmt|1', 'MyDotsTranslator:...\\excluded'),
      src = 'MySrcTranslator:1'
    )
  )
  expect_equal(
    message_data$msgid,
    c(
      "A default message", "you found me!", "a translated", "argument",
      "A default message", "a standard src message", "An untranslated string"
    )
  )

  message_data = get_message_data(
    test_package("custom_translation"),
    custom_translation_functions = list(
      R = c('MyOtherTranslator:target_arg|2', 'MyPluralTranslator:singular|3,plural|4'),
      src = 'MyArg4Translator:4'
    )
  )
  expect_equal(
    message_data$msgid,
    c(
      "A default message", "A default message", "you found me too!", "found by position",
      NA_character_, NA_character_, "a standard src message", "Another untranslated string"
    )
  )
  expect_equal(
    message_data$msgid_plural,
    list(NULL, NULL, NULL, NULL, c("singular", "plural"), c("another singular", "another plural"), NULL, NULL)
  )
})

test_that("faulty custom_translation_functions specs error", {
  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(src = "abc:def")
    ),
    "All inputs for src must be key-value pairs like fn:arg1", fixed = TRUE
  )

  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(R = "abc")
    ),
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.", fixed = TRUE
  )

  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(R = "abc:def:ghi")
    ),
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.", fixed = TRUE
  )

  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(R = "abc:def")
    ),
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.", fixed = TRUE
  )
})
