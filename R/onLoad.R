globalVariables(
  c(
    'KNOWN_LANGUAGES', 'PLURAL_RANGE_STRINGS',
    'msgid', 'msgstr', 'x.msgstr', 'msgid_plural', 'msgstr_plural', 'x.msgstr_plural',
    'message_source', 'type', 'line_number', 'fuzzy', 'x.fuzzy',
    'is_repeat', 'is_marked_for_translation',
    'id', 'x.id', 'i.id', 'line1', 'i.line1', 'i.col1', 'i.line2', 'i.col2',
    'text', 'x.text', 'i.text', 'x.token', 'token', 'fname', 'x.fname', 'i.fname',
    'parent', 'x.parent', 'i.parent', 'ancestor', 'i.ancestor',
    'call_expr', 'call_id', 'call_expr_id', 'call_parent_id', 'arg_name', 'arg_value', 'lines',
    'source_location', 'c_fmt_tag', 'msgid_plural_str', 'msgstr_plural_str', 'suggested_call',
    '.'
  ),
  package = 'potools'
)

.potools = new.env()

if (requireNamespace('crayon', quietly = TRUE)) {
  call_color = getOption('potools.call_color', crayon::green)
  file_color = getOption('potools.file_color', crayon::white)
  msgid_color = getOption('potools.msgid_color', crayon::red)
  language_color = getOption('potools.language_color', crayon::cyan)
  replacement_color = getOption('potools.replacement_color', crayon::blue)
  plural_range_color = getOption('potools.plural_range_color', crayon::yellow)
} else {
  call_color = file_color = msgid_color = language_color = replacement_color = plural_range_color = identity
}

.onLoad = function(libname, pkgname) {
  .potools$base_package_names = get(".get_standard_package_names", envir=asNamespace("tools"), mode="function")()$base
}
