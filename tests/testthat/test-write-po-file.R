# TODO: a better job here. I am really unmotivated to write these at the moment. Should have started from here...
test_that("po_metadata constructor & methods work", {
  metadata <- po_metadata(
    package = 'test', version = '0.0.1', language='da',
    copyright = ''
  )
  expect_s3_class(metadata, "po_metadata")

  expect_match(format(metadata), '"Project-Id-Version: test 0.0.1\\n"', fixed=TRUE)
  expect_match(format(metadata), '"PO-Revision-Date: [0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}[-+][0-9]{4}')
  expect_all_match(
    format(metadata, template = TRUE),
    c(
      '"Language-Team: LANGUAGE <LL@li.org>\\n"',
      '"Content-Type: text/plain; charset=CHARSET\\n"',
      '# SOME DESCRIPTIVE TITLE.',
      '# This file is distributed under the same license as the R package.',
      '# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.'
    ),
    fixed=TRUE
  )
  expect_match(format(metadata, use_plurals=TRUE), '"Plural-Forms: nplurals=2; plural=(n!=1);\\n"', fixed=TRUE)

  expect_output(print(metadata), '"Project-Id-Version: test 0.0.1\\n"', fixed=TRUE)
})

# TODO: migrate some tests from test-translate-package to here
test_that("write_po_file works", {
  message_data <- get_message_data(test_package("r-devel/src/library/base"))[message_source == 'src']
  metadata <- po_metadata(package = "base", version = '0.0.1')

  tmp <- tempfile(fileext = '.pot')
  on.exit(unlink(tmp))
  write_po_file(message_data, tmp, metadata)
  expect_all_match(
    readLines(tmp),
    c('msgid "A string"', 'msgid_plural "Some strings"', 'msgstr[0] ""', 'msgstr[1] ""'),
    fixed = TRUE
  )
})
