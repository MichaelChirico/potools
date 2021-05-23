globalVariables(
  c(
    'KNOWN_LANGUAGES', 'PLURAL_RANGE_STRINGS',
    'msgid', 'msgstr', 'x.msgstr', 'plural_msgid', 'plural_msgstr', 'x.plural_msgstr',
    'message_source', 'type', 'fuzzy', 'x.fuzzy', 'is_repeat', '.'
  ),
  package = 'potools'
)

.potools = new.env()

if (requireNamespace('crayon', quietly = TRUE)) {
  call_color = getOption('potools.call_color', crayon::green)
  file_color = getOption('potools.file_color', crayon::white)
  msgid_color = getOption('potools.msgid_color', crayon::red)
  language_color = getOption('potools.language_color', crayon::cyan)
  build_gettextf_color = getOption('potools.build_gettextf_color', crayon::blue)
  plural_range_color = getOption('potools.plural_range_color', crayon::yellow)
} else {
  call_color = file_color = msgid_color = language_color = build_gettextf_color = plural_range_color = identity
}
