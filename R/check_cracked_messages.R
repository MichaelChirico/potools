# check for calls like stop("a", i, "b", j) that are better suited for
#   translation as calls like gettextf("a%db%d", i, j)
check_cracked_messages = function(message_data, package, verbose) {
  exit =
    message_data[type == 'singular', if (.N > 1L) .(msgid), by=.(file, call)
                 ][ , {
                   if (.N > 0L && verbose) message(domain=NA, gettextf(
                     'Found %d messaging calls that might be better suited for gettextf for ease of translation:',
                     uniqueN(call), domain='R-potools'
                   ))
                   .SD
                 }][ , by = .(file, call), {
                   cat(gettextf(
                     '\nMulti-string call:\n%s\n< File:%s >\nPotential replacement with gettextf():\n%s\n',
                     call_color(.BY$call),
                     file_color(.BY$file),
                     build_gettextf_color(build_gettextf_call(.BY$call, package))
                   ))
                   TRUE
                 }][ , if (.N > 0L) prompt('Exit now to repair any of these? [y/N]') else 'n']
  return(tolower(exit))
}
