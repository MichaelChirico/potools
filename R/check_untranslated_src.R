# check a package for character arrays in messaging functions that
#   haven't been passed through the translation macro
# TODO: I think we can merge this with check_untranslated_cat by pulling
#   strings from cat during get_r_messages() and just marking those as
#   !is_marked_for_translation at that point.
check_untranslated_src <- function (message_data) {
  unmarked_messages = message_data[message_source == "src" & !is_marked_for_translation]
  if (!nrow(unmarked_messages)) return('n')

  message(domain=NA, gettextf(
    'Found %d src messaging calls that were not properly marked for translation:',
    nrow(unmarked_messages)
  ))
  for (ii in seq_len(nrow(unmarked_messages))) {
    unmarked_messages[ii, cat(gettextf(
      '\nUntranslated call:\n%s\n< File:%s, Line:%s >\n',
      call_color(call),
      file_color(file),
      file_color(line_number)
    ))]
  }

  exit = prompt('Exit now to repair any of these? [y/N]')
  return(tolower(exit))
}
