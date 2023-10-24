test_that("po_explain_plurals works as expected", {
  # NB: expect_messages since technically message() can be called >1 time internally
  expect_messages(
    po_explain_plurals("ru"),
    c("Russian.*3 plural forms", "plural_index = 0 applies when n = 1")
  )

  expect_message(po_explain_plurals("ar", 3), "Arabic.*plural index 3 applies when n = 3-10")

  # known error cases
  expect_error(po_explain_plurals(1L), "Supply one language code", fixed=TRUE)
  expect_error(po_explain_plurals(letters), "Supply one language code", fixed=TRUE)
  expect_error(po_explain_plurals("en", "a"), "If supplied, `index` should be a single non-negative number", fixed=TRUE)
  expect_error(
    po_explain_plurals("en", 1:10),
    "If supplied, `index` should be a single non-negative number",
    fixed=TRUE
  )
  expect_error(po_explain_plurals("en", -1), "If supplied, `index` should be a single non-negative number", fixed=TRUE)

  expect_error(po_explain_plurals("xx"), "not a known language", fixed=TRUE)
  expect_error(po_explain_plurals("en", 10L), "en only has 2 plural forms", fixed=TRUE)
})
