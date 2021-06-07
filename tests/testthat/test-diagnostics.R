# TODO: can we refactor/eliminate anything from test-translate-package to just be done here instead of in
#   the more circuitous tests there?
test_that("check_cracked_messages works", {
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


})
