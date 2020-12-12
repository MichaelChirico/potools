# readline on steroids.
#  - all the points in utils.R about prompt() apply
#  - we want to unescape strings. the encodeString version
#    should be treated as an "implementation detail" -- we
#    show the user the most recognizable version of it.
#    it might make sense to just store two columns --
#    e.g. msgid and msgid_plain, but this doubles the number
#    of columns. the number of messages is not large enough
#    in any package I've seen (base and data.table are
#    both < 2000), so the advantage of vectorizing is not
#    expected to be noticeable
#  - we want to help users get templates right (which can be
#    tough).
read_translation = function(msgid, type, file, call, metadata) {
  msgid = unescape_string(msgid)
  if (type == 'plural') {
    translation <- character(metadata$nplurals)
    for (jj in seq_len(metadata$nplurals)) {
      translation[jj] = prompt(gettextf(
        '\nFile: %s\nCall: %s\nPlural message: "%s"\nHow would you translate this message into %s %s?',
        white(file),
        green(call),
        red(msgid),
        blue(metadata$full_name_eng),
        yellow(PLURAL_RANGE_STRINGS[.(metadata$plural, jj-1L), range]),
        domain = "R-potools"
      ))
    }
  } else {
    prompt(gettextf(
      '\nFile: %s\nCall: %s\nMessage: "%s"\nHow would you translate this message into %s?',
      white(file),
      green(call),
      red(msgid),
      blue(metadata$full_name_eng),
      domain = "R-potools"
    ))
  }
}
