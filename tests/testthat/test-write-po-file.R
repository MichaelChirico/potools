# TODO: a better job here. I am really unmotivated to write these at the moment. Should have started from here...
test_that("po_metadata constructor & methods work", {
  metadata <- po_metadata(package = 'test', version = '0.0.1', language='da')
  expect_s3_class(metadata, "po_metadata")

  expect_match(format(metadata), '"Project-Id-Version: test 0.0.1\\n"', fixed=TRUE)
  expect_match(format(metadata, template=TRUE), '"Language-Team: LANGUAGE <LL@li.org>\\n"', fixed=TRUE)
  expect_match(format(metadata, use_plurals=TRUE), '"Plural-Forms: nplurals=2; plural=(n!=1);\\n"', fixed=TRUE)

  expect_output(print(metadata), '"Project-Id-Version: test 0.0.1\\n"', fixed=TRUE)
})

# TODO: migrate some tests from test-translate-package to here
# test_that("write_po_file works", {
#
# })
