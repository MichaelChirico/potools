# check for calls like stop("a", i, "b", j) that are better suited for
#   translation as calls like gettextf("a%db%d", i, j)
check_cracked_messages = function(message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)

  # check for , as a crude filter to avoid parsing too many calls
  dup_messages = message_data[
    !is_repeat
    & is_marked_for_translation
    & message_source == "R"
    & type == 'singular'
    & grepl(",", call, fixed = TRUE),
    # line_number is sorted; use head() for 0-length edge case
    .(line_number = head(line_number, 1L)),
    by=c('file', 'call')
  ]
  if (!nrow(dup_messages)) return(diagnostic_schema())

  dup_messages[ , 'call_expr' := lapply(call, str2lang)]
  dup_messages = dup_messages[count_dots(call_expr) > 1L]
  if (!nrow(dup_messages)) return(diagnostic_schema())

  for (ii in seq_len(nrow(dup_messages))) {
    set(dup_messages, ii, "replacement", build_gettextf_call(dup_messages$call_expr[[ii]]))
  }

  return(dup_messages[ , .(call, file, line_number, replacement)])
}
attr(check_cracked_messages, "diagnostic_tag") =
  "R messaging calls that might be better suited for gettextf for ease of translation"

# take a call like stop("a", i, "b", j) and suggest
#   stop(domain=NA, gettextf("a%sb%s", i, j))
build_gettextf_call = function(e) {
  sprintf(
    '%s(domain=NA, %s)',
    as.character(e[[1L]]),
    gettextify(e[-1L])
  )
}

# count the number of ... arguments by excluding named arguments; uses a match.call()
#   and a get() to the function definition, which is relatively expensive, so done carefully/sparingly.
count_dots = function(call) {
  fname = vapply(
    call,
    # get_r_messages should ensure that one of these two conditions will return a scalar
    function(x) if (is.name(x[[1L]])) as.character(x[[1L]]) else as.character(x[[1L]][[3L]]),
    character(1L)
  )
  DT = data.table(call, fname)
  DT[ , 'n_args' := 0L]
  DT[
    fname %chin% c("stop", "warning", "message", "packageStartupMessage") & lengths(call) > 2L,
    by = fname,
    'n_args' := {
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
    }
  ]
  DT$n_args
}
