globalVariables(
  c(
    'msgid', 'msgstr', 'x.msgstr', 'msgid_plural', 'msgstr_plural', 'x.msgstr_plural',
    'message_source', 'type', 'line_number', 'fuzzy', 'x.fuzzy',
    'is_repeat', 'is_marked_for_translation', 'is_templated',
    'id', 'x.id', 'i.id', 'line1', 'i.line1', 'x.line1', 'i.col1', 'i.line2', 'i.col2',
    'text', 'x.text', 'i.text', 'x.token', 'token', 'fname', 'x.fname', 'i.fname',
    'parent', 'x.parent', 'i.parent', 'ancestor', 'i.ancestor',
    'call_expr', 'call_id', 'call_expr_id', 'call_parent_id', 'arg_name', 'arg_value', 'lines',
    'source_location', 'c_fmt_tag', 'msgid_plural_str', 'msgstr_plural_str', 'suggested_call',
    'array_start', 'i.array_start', 'array_end', 'i.array_end', 'call_start', 'x.call_start', 'i.call_start',
    'paren_start', 'i.paren_start', 'paren_end', 'i.paren_end', 'x.paren_end',
    'i.msgid', 'replacement', 'non_spurious', 'str_arg',
    'start', 'start.x', 'end', 'special', 'redirect_start', 'redirect_length', 'id_start', 'id_length',
    'width_precision', 'width_precision_start', 'width_precision_length', 'by_id',
    'lparen', 'N.x', 'N.y', 'next_paren', 'next_start',
    'nplurals', 'full_name_eng', 'full_name_native', 'plural_index',
    '.', 'N'
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

# nocov start
.onLoad = function(libname, pkgname) {
  .potools$base_package_names = get(".get_standard_package_names", envir=asNamespace("tools"), mode="function")()$base
}
# nocov end
