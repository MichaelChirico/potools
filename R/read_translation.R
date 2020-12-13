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
read_translation = function(msgid, type, file, call, fuzzy, msgstr, metadata) {
  msgid = unescape_string(msgid)
  # NB: it's tempting to vectorize running get_fmt to e.g.
  #   add a new column msgid_fmt to message_data. But
  #   unescape_string here dashes hopes that could work.
  #   Moreover we can't simply get the template _just prior_
  #   to running escape_string in get_r_messages, because
  #   escape_string and unescape_string are (intentionally)
  #   not inverses. Bummer. So run it here, to make sure
  #   the match positions are relative to the string that
  #   will be shown to the user.
  specials = get_specials(msgid)[[1L]]
  if (length(specials)) {
    special_tags = get_special_tags(msgid, specials)
  } else {
    special_tags = ""
  }
  n_format = count_formats(msgid)
  if (type == 'plural') {
    translation <- character(metadata$nplurals)
    # add enough blanks for Plural message:
    if (nzchar(special_tags)) special_tags = paste0("\n                ", special_tags)
    for (jj in seq_len(metadata$nplurals)) {
      if (fuzzy == 1L) {
        fuzzy_tag = gettextf(
          "\n **Note: a similar message was previously translated as: **\n%s",
          msgstr[[jj]], domain='R-potools'
        )
      } else {
        fuzzy_tag = ""
      }
      translation[jj] = prompt_with_templates(n_format, gettextf(
        '\nFile: %s\nCall: %s\nPlural message: %s%s\nHow would you translate this message into %s %s?%s',
        file_color(file),
        call_color(call),
        msgid_color(msgid),
        special_tags,
        language_color(metadata$full_name_eng),
        plural_range_color(PLURAL_RANGE_STRINGS[.(metadata$plural, jj-1L), range]),
        fuzzy_tag,
        domain = "R-potools"
      ))
    }
  } else {
    # add enough blanks for Message:
    if (nzchar(special_tags)) special_tags = paste0("\n         ", special_tags)
    if (fuzzy == 1L) {
      fuzzy_tag = gettextf(
        "\n **Note: a similar message was previously translated as: **\n%s",
        msgstr, domain='R-potools'
      )
    } else {
      fuzzy_tag = ""
    }
    translation = prompt_with_templates(n_format, gettextf(
      '\nFile: %s\nCall: %s\nMessage: %s%s\nHow would you translate this message into %s?%s',
      file_color(file),
      call_color(call),
      msgid_color(msgid),
      special_tags,
      language_color(metadata$full_name_eng),
      fuzzy_tag,
      domain = "R-potools"
    ))
  }
  translation
}
