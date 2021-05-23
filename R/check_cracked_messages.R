# check for calls like stop("a", i, "b", j) that are better suited for
#   translation as calls like gettextf("a%db%d", i, j)
check_cracked_messages = function(message_data, package) {

  dup_messages = message_data[
    message_source == "R" & type == 'singular',
    if (.N > 1L) TRUE,
    by=c('file', 'call')
  ]
  if (!nrow(dup_messages)) return('n')

  message(domain=NA, gettextf(
    'Found %d R messaging calls that might be better suited for gettextf for ease of translation:',
    nrow(dup_messages)
  ))

  for (ii in seq_len(nrow(dup_messages))) {
    dup_messages[ii, cat(gettextf(
      '\nMulti-string call:\n%s\n< File:%s >\nPotential replacement with gettextf():\n%s\n',
      call_color(call[ii]),
      file_color(file[ii]),
      build_gettextf_color(build_gettextf_call(call[ii]))
    ))]
  }

  exit = prompt('Exit now to repair any of these? [y/N]')
  return(tolower(exit))
}
