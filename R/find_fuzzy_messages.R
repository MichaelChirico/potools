find_fuzzy_messages <- function(message_data, lang_file) {
  old_message_data = get_po_messages(lang_file)

  if (any(idx <- old_message_data$fuzzy == 2L)) {
    messagef('Found %d translations marked as deprecated in %s.', sum(idx), lang_file)
    message('Typically, this means the corresponding error messages have been refactored.')
    message('Reproducing these messages here for your reference since they might still provide some utility.')

    dashes = strrep('-', .9*getOption('width'))
    old_message_data[idx & type == 'singular', {
      if (.N > 0L) {
        message(' ** SINGULAR MESSAGES **')
        cat(rbind(dashes, msgid, msgstr), sep='\n')
      }
    }]
    old_message_data[idx & type == 'plural', {
      if (.N > 0L) {
        message(' ** PLURAL MESSAGES **')
        cat(do.call(rbind, c(list(dashes), msgid_plural, msgstr_plural)), sep='\n')
      }
    }]

    old_message_data = old_message_data[(!idx)]
  }

  msg_src <- if (startsWith(basename(lang_file), "R-")) "R" else "src"

  # filter 'type' here to make sure we can only assign to those rows
  message_data[
    message_source == msg_src & type == 'singular',
    c("msgstr", "fuzzy") := old_message_data[.SD, on = c("type", "msgid"), .(x.msgstr, x.fuzzy)]
  ]
  # can't join on lists :\
  if (!all(vapply(old_message_data$msgstr_plural, is.null, logical(1L)))) {
    message_data[
      message_source == msg_src,
      'join_id' := vapply(msgid_plural, paste, character(1L), collapse='|||')
    ]
    old_message_data[
      message_source == msg_src,
      'join_id' := vapply(msgid_plural, paste, character(1L), collapse='|||')
    ]
    message_data[
      message_source == msg_src & type == 'plural',
      c("msgstr_plural", "fuzzy") := old_message_data[.SD, on = c('type', 'join_id'), .(x.msgstr_plural, x.fuzzy)]
    ]

    message_data[ , 'join_id' := NULL]
    old_message_data[ , 'join_id' := NULL]
  }
}
