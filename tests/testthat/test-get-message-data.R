test_that("Packages with src code & C syntax errors fail gracefully", {
  expect_error(
    get_message_data(test_package("r_src_err_1")),
    "Parsing error: found an odd number (3)", fixed = TRUE
  )
  # TODO(#209): reactivate these. See discussion in #199 -- for now, the simplest
  #   way forward is to accept some misbehavior on erroneous C files that won't parse,
  #   and leave it to users to get their C files compiling first before running
  #   get_message_data().
  # expect_error(
  #   get_message_data(test_package("r_src_err_2")),
  #   "Parsing error: unmatched parentheses", fixed = TRUE
  # )
  # expect_error(
  #   get_message_data(test_package("r_src_err_3")),
  #   "Parsing error: unmatched parentheses", fixed = TRUE
  # )
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
  expect_identical(
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
  expect_identical(
    message_data$msgid,
    c(
      "A default message", "A default message", "you found me too!", "found by position",
      NA_character_, NA_character_, "a standard src message", "Another untranslated string"
    )
  )
  expect_identical(
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
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
    fixed = TRUE
  )

  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(R = "abc:def:ghi")
    ),
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
    fixed = TRUE
  )

  expect_error(
    get_message_data(
      test_package("custom_translation"),
      custom_translation_functions = list(R = "abc:def")
    ),
    "All inputs for R must be key-value pairs like fn:arg1|n1[,arg2|n2] or fn:...\\arg1,...,argn.",
    fixed = TRUE
  )
})

test_that("Message exclusions are respected", {
  expect_all_match(
    get_message_data(test_package("r_msg"))$msgid,
    c("skip me for translation", "me too", "me three"),
    fixed = TRUE, invert = TRUE
  )

  # C-level exclusions
  expect_all_match(
    get_message_data(test_package("r_src_c"))$msgid,
    c("Watch me disappear", "Like a ghost", "Into thin air"),
    fixed = TRUE, invert = TRUE
  )

  # mismatch of start/end counts in a file
  expect_error(
    get_message_data(test_package("r_err_1")),
    "Invalid # notranslate start/end.*start\\(s\\)"
  )
  # end comes before start, so there's a mismatch even if the counts are the same
  expect_error(
    get_message_data(test_package("r_err_2")),
    "Invalid # notranslate start/end.*Unmatched"
  )
})

test_that("Pre-processor macros don't break parentheses matching", {
  # solution is hacky, but this test at least helps prevent regression going forward
  expect_identical(get_message_data(test_package("unusual_msg"))[file == 'z.c']$msgid, "You found me!")
})
