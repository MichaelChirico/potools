# check for calls like stop("a", i, "b", j) that are better suited for
#   translation as calls like gettextf("a%db%d", i, j)
check_cracked_messages = function(message_data) {

  dup_messages = unique(
    # check for , as a crude filter to avoid parsing too many calls
    message_data[!is_repeat & message_source == "R" & type == 'singular' & grepl(",", call, fixed = TRUE)],
    by=c('file', 'call')
  )
  if (!nrow(dup_messages)) return('n')

  dup_messages[ , call_expr := lapply(call, str2lang)]
  dup_messages = dup_messages[count_dots(call_expr) > 1L]

  if (!nrow(dup_messages)) return('n')

  message(domain=NA, gettextf(
    'Found %d R messaging calls that might be better suited for gettextf for ease of translation:',
    nrow(dup_messages)
  ))

  for (ii in seq_len(nrow(dup_messages))) {
    dup_messages[ii, cat(gettextf(
      '\nMulti-string call:\n%s\n< File:%s >\nPotential replacement with gettextf():\n%s\n',
      call_color(call),
      file_color(file),
      build_gettextf_color(build_gettextf_call(call_expr))
    ))]
  }

  exit = prompt('Exit now to repair any of these? [y/N]')
  return(tolower(exit))
}

# count the number of ... arguments by excluding named arguments; uses a match.call()
#   and a get() to the function definition, which is relatively expensive, so done carefully/sparingly.
count_dots = function(call) {
  fname = vapply(call, function(x) as.character(x[[1L]]), character(1L))
  DT = data.table(call, fname)
  DT[ , n_args := 0L]
  DT[!fname %chin% c("gettext", "gettextf") & lengths(call) > 2L, by = fname, n_args := {
    definition = get(.BY$fname)
    vapply(
      call,
      function(x) {
        e = match.call(definition, x)
        if (is.null(names(e))) return(length(e) - 1L)
        return(length(e) - sum(names(e) %chin% c("call.", "immediate.", "noBreaks.", "domain", "appendLF")) - 1L)
      },
      integer(1L)
    )
  }]
  DT$n_args
}
