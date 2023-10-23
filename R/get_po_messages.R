# extract all the messages from a po file as a table matching the structure
#   found from get_r_messages, namely with 3 columns: type, msgid, plural_msg.
#   two columns (file, call) are missing because (at least for R files)
#   these are not recorded by gettext.
# TODO: can we just wrap libgettextpo?
#   https://www.gnu.org/software/gettext/manual/gettext.html#libgettextpo
get_po_messages <- function(po_file) {
  po_lines = readLines(po_file, encoding="UTF-8")
  message_source = if (startsWith(basename(po_file), "R-")) "R" else "src"
  po_length = length(po_lines)

  if (po_length == 0L) {
    return(data.table(
      message_source = character(),
      type = character(),
      fuzzy = integer(),
      msgid = character(),
      msgstr = character(),
      msgid_plural = vector('list'),
      msgstr_plural = vector('list')
    ))
  }

  # number of msgstr corresponding to each msgid_plural depends on
  #   the language (e.g. just one for Chinese, but 3 for Polish).
  # the number of rows in the output is:
  #   (1) count of msgid/msgstr pairs (excluding msgid paired with msgid_plural)
  #   (2) count of msgid_plural
  # anchor to ^ to skip fuzzied messages
  msgid_start = grep("^msgid ", po_lines)
  n_msgid = length(msgid_start)
  msgstr_start = grep("^msgstr ", po_lines)

  # land on the msgid coming just before each msgid_plural
  msgid_plural_start = msgid_start[findInterval(grep("^msgid_plural", po_lines), msgid_start)]
  # TODO: maybe just split() or tapply() these with findInterval and iterate instead
  #   of the while loop below which essentially re-finds these?
  msgstr_plural_start = grep("^msgstr\\[", po_lines)

  # now trim any such msgid so that all msgid_start correspond to singular messages
  msgid_start = setdiff(msgid_start, msgid_plural_start)
  n_singular = length(msgid_start)
  n_plural = length(msgid_plural_start)
  n_msgstr_plural = length(msgstr_plural_start)

  if (n_singular != length(msgstr_start)) {
    stopf("Found %d msgid which differs from %d msgstr; corrupted .po file", n_singular, length(msgstr_start))
  }

  if ((n_plural == 0L && n_msgstr_plural > 0L) || (n_plural > 0L && n_msgstr_plural %% n_plural != 0L)) {
    stopf(
      "Found %d msgid_plural, which does not evenly divide %d msgstr[n]; corrupted .po file",
      n_msgstr_plural, n_plural
    )
  }
  # pre-calculate which lines contain message continuations. Append
  #   FALSE for a while loop to terminate gracefully on hitting file end
  is_msg_continuation = c(startsWith(po_lines, '"'), FALSE)

  po_data = data.table(
    message_source = message_source,
    type = rep(c("singular", "plural"), c(n_singular, n_plural)),
    fuzzy = integer(n_msgid),
    msgid = character(n_msgid),
    msgstr = character(n_msgid),
    msgid_plural = vector('list'),
    msgstr_plural = vector('list')
  )
  # may not have been caught above if the file is all fuzzy translations, e.g.
  if (n_msgid == 0L) return(po_data)

  # inherits is_msg_continuation
  find_msg_end <- function(start_idx) {
    # returns the first FALSE found. since tail(.,1) is FALSE,
    #   guaranteed to give a match.
    # -start_idx includes skipping start_idx itself, so we won't
    #   end at the first line
    start_idx + match(FALSE, tail(is_msg_continuation, -start_idx)) - 1L
  }
  # inherits polines
  build_msg <- function(start, end, tag) {
    paste(gsub(sprintf('^(?:%s +)?"|"$', tag), '', po_lines[start:end]), collapse = '')
  }

  msg_j = 1L
  while (msg_j <= length(msgid_start)) {
    start = msgid_start[msg_j]
    end = find_msg_end(start)
    set(po_data, msg_j, 'msgid', build_msg(start, end, 'msgid'))

    set(po_data, msg_j, 'fuzzy', as.integer(start != 1L && startsWith(po_lines[start - 1L], "#, fuzzy")))

    start = end + 1L
    end = find_msg_end(start)
    set(po_data, msg_j, 'msgstr', build_msg(start, end, 'msgstr'))
    msg_j = msg_j + 1L
  }

  plural_i = 1L
  while (plural_i <= length(msgid_plural_start)) {
    start = msgid_plural_start[plural_i]
    end = find_msg_end(start)
    msg1 = build_msg(start, end, 'msgid')

    set(po_data, msg_j, 'fuzzy', as.integer(start != 1L && startsWith(po_lines[start - 1L], "#, fuzzy")))

    start = end + 1L
    end = find_msg_end(start)
    msg2 = build_msg(start, end, 'msgid_plural')

    set(po_data, msg_j, 'msgid_plural', list(c(msg1, msg2)))

    start = end + 1L
    msgstr_plural = character()
    while (start <= po_length && startsWith(po_lines[start], 'msgstr[')) {
      end = find_msg_end(start)
      msgstr_plural = c(msgstr_plural, build_msg(start, end, 'msgstr\\[\\d+\\]'))
      start = end + 1L
    }
    set(po_data, msg_j, 'msgstr_plural', list(msgstr_plural))

    plural_i = plural_i + 1L
    msg_j = msg_j + 1L
  }

  # somewhat hacky approach -- strip the comment markers & recurse
  writeLines(
    gsub("^#~ ", "", grep("^#~ ", po_lines, value = TRUE)),
    tmp <- tempfile()
  )
  on.exit(unlink(tmp))
  deprecated = get_po_messages(tmp)
  if (nrow(deprecated) > 0L) {
    set(deprecated, NULL, 'fuzzy', 2L)
    po_data = rbind(po_data, deprecated)
  }

  po_data[]
}
