find_fuzzy_messages <- function(message_data, lang_file) {
  old_message_data = get_po_messages(lang_file)

  if (any(idx <- old_message_data$fuzzy == 2L)) {
    message(domain=NA, gettextf(
      'Found %d translations marked as deprecated in %s.',
      sum(idx), lang_file
    ))
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
        cat(do.call(rbind, c(list(dashes), plural_msgid, plural_msgstr)), sep='\n')
      }
    }]

    old_message_data = old_message_data[(!idx)]
  }

  message_data[
    old_message_data[type == 'singular'],
    on = c('type', 'msgid'),
    `:=`(msgstr = i.msgstr, fuzzy = i.fuzzy)
  ]
  # can't join on lists :\
  if (!all(vapply(old_message_data$plural_msgstr, is.null, logical(1L)))) {
    message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
    old_message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
    message_data[
      old_message_data[type == 'plural'],
      on = c('type', 'join_id'),
      `:=`(plural_msgstr = i.plural_msgstr, fuzzy = i.fuzzy)
    ]

    message_data[ , 'join_id' := NULL]
    old_message_data[ , 'join_id' := NULL]
  }
}
