# extract all the messages from a po file as a table matching the structure
#   found from get_r_messages, namely with 3 columns: type, msgid, plural_msg.
#   two columns (file, call) are missing because (at least for R files)
#   these are not recorded by gettext.
get_po_messages <- function(po_file) {
  if (!file.exists(po_file)) stop(domain=NA, gettextf("File not found: %s", po_file, domain="R-potools"))
  po_lines = readLines(po_file)
  po_length = length(po_lines)

  # TODO: need a new column, fuzzy. Two cases:
  #  (1) fuzzy guesses -- #, fuzzy appears in the line before msgid, msgid not commented
  #  (2) deprecations -- #, fuzzy appears in the line before #~ msgid

  # number of msgstr corresponding to each msgid_plural depends on
  #   the language (e.g. just one for Chinese, but 3 for Polish).
  # the number of rows in the output is:
  #   (1) count of msgid/msgstr pairs (excluding msgid paired with msgid_plural)
  #   (2) count of msgid_plural
  # anchor to ^ to skip fuzzied messages
  msgid_start = grep("^msgid ", po_lines)
  msgstr_start = grep("^msgstr ", po_lines)

  # land on the msgid coming just before each msgid_plural
  plural_msgid_start = msgid_start[findInterval(grep("^msgid_plural", po_lines), msgid_start)]
  # TODO: maybe just split() or tapply() these with findInterval and iterate instead
  #   of the while loop below which essentially re-finds these?
  plural_msgstr_start = grep("^msgstr\\[", po_lines)

  # now trim any such msgid so that all msgid_start correspond to singular messages
  msgid_start = setdiff(msgid_start, plural_msgid_start)
  n_singular = length(msgid_start)
  n_plural = length(plural_msgid_start)

  if (n_singular != length(msgstr_start)) {
    stop(domain=NA, gettextf(
      "Found %d msgid which differs from %d msgstr; corrupted .po file",
      n_singular, length(msgstr_start), domain="R-potools"
    ))
  }

  if (length(plural_msgstr_start) %% n_plural != 0L) {
    stop(domain=NA, gettextf(
      "Found %d msgid_plural, which does not evenly divide %d msgstr[n]; corrupted .po file",
      length(plural_msgstr_start), n_plural, domain="R-potools"
    ))
  }
  # pre-calculate which lines contain message continuations. Append
  #   FALSE for a while loop to terminate gracefully on hitting file end
  is_msg_continuation = c(grepl('^"', po_lines), FALSE)

  n_msg = n_singular + n_plural
  po_data = data.table(
    type = rep(c("singular", "plural"), c(n_singular, n_plural)),
    msgid = character(n_msg),
    msgstr = character(n_msg),
    plural_msgid = vector('list'),
    plural_msgstr = vector('list')
  )
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
    paste(gsub(sprintf('^(?:%s )?"|"$', tag), '', po_lines[start:end]), collapse = '')
  }

  msg_j = 1L
  while (msg_j <= length(msgid_start)) {
    start = msgid_start[msg_j]
    end = find_msg_end(start)
    set(po_data, msg_j, 'msgid', build_msg(start, end, 'msgid'))

    start = end + 1L
    end = find_msg_end(start)
    po_data[msg_j, 'msgstr' := build_msg(start, end, 'msgstr')]
    msg_j = msg_j + 1L
  }

  plural_i = 1L
  while (plural_i <= length(plural_msgid_start)) {
    start = plural_msgid_start[plural_i]
    end = find_msg_end(start)
    msg1 = build_msg(start, end, 'msgid')

    start = end + 1L
    end = find_msg_end(start)
    msg2 = build_msg(start, end, 'msgid_plural')

    set(po_data, msg_j, 'plural_msgid', list(list(msg1, msg2)))

    start = end + 1L
    plural_msgstr = list()
    while (start <= po_length && grepl('^msgstr\\[', po_lines[start])) {
      end = find_msg_end(start)
      plural_msgstr = c(plural_msgstr, list(build_msg(start, end, 'msgstr\\[\\d+\\]')))
      start = end + 1L
    }
    set(po_data, msg_j, 'plural_msgstr', list(plural_msgstr))

    plural_i = plural_i + 1L
    msg_j = msg_j + 1L
  }

  # do here to vectorize over messages
  po_data[type == 'singular', c('msgid', 'msgstr') := .(unescape_str(msgid), unescape_str(msgstr))]
  po_data[type == 'plural', `:=`(
    plural_msgid = lapply(plural_msgid, unescape_str),
    plural_msgstr = lapply(plural_msgstr, unescape_str)
  )]
  po_data[]
}
