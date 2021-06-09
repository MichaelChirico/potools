# TODO: can we refactor/eliminate anything from test-translate-package to just be done here instead of in
#   the more circuitous tests there?
test_that("check_cracked_messages works", {
  message_data = data.table::data.table(
    message_source = 'R',
    type = 'singular',
    file = 'foo.R',
    msgid = c('hello', 'farewell', 'sir', 'found', 'problems'),
    msgid_plural = list(NULL),
    call = c('stop("hello")', rep('message("farewell", "sir")', 2L), rep('warning("found ", nn, " problems")', 2L)),
    line_number = 1:5,
    is_repeat = FALSE,
    is_marked_for_translation = TRUE
  )

  expect_equal(
    check_cracked_messages(message_data),
    data.table(
      call = c('message("farewell", "sir")', 'warning("found ", nn, " problems")'),
      file = 'foo.R',
      line_number = c(2L, 4L),
      replacement = c(
        'message(domain=NA, gettext("farewellsir"))',
        'warning(domain=NA, gettextf("found %s problems", nn))'
      )
    )
  )

  # input that can be converted to data.table is OK
  expect_equal(check_cracked_messages(as.data.frame(message_data)), check_cracked_messages(message_data))
})

test_that("check_untranslated_cat works", {
  message_data = data.table::data.table(
    message_source = 'R',
    type = 'singular',
    file = 'foo.R',
    msgid = c('hello', 'farewell', 'sir', 'this is not'),
    msgid_plural = list(NULL),
    call = c('cat("hello")', rep('cat("farewell", "sir")', 2L), 'cat(gettext("this is translated"), "this is not")'),
    line_number = 1:4,
    is_repeat = FALSE,
    is_marked_for_translation = FALSE
  )

  expect_equal(
    check_untranslated_cat(message_data),
    data.table(
      call = c('cat("hello")', 'cat("farewell", "sir")'),
      file = 'foo.R',
      line_number = 1:2,
      replacement = c('cat(gettext("hello"))', 'cat(gettext("farewell sir"))')
    )
  )

  # input that can be converted to data.table is OK
  expect_equal(check_untranslated_cat(as.data.frame(message_data)), check_untranslated_cat(message_data))
})

test_that("check_untranslated_src works", {
  message_data = data.table::data.table(
    message_source = 'src',
    type = 'singular',
    file = 'bar.c',
    msgid = c('Found an issue', 'Impossible!'),
    msgid_plural = list(NULL),
    call = c('Rprintf("Found an issue", s)', 'error(_("Impossible!"))'),
    line_number = 1:2,
    is_repeat = FALSE,
    is_marked_for_translation = c(FALSE, TRUE)
  )

  expect_equal(
    check_untranslated_src(message_data),
    data.table(
      call = c('Rprintf("Found an issue", s)'),
      file = 'bar.c',
      line_number = 1L,
      replacement = NA_character_
    )
  )

  # input that can be converted to data.table is OK
  expect_equal(check_untranslated_src(as.data.frame(message_data)), check_untranslated_src(message_data))
})
