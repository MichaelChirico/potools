# extract all the messages from a po file as a table matching the structure
#   found from get_r_messages, namely with 3 columns: type, plural_index, msgid.
#   two columns (file, call) are missing because (at least for R files)
#   these are not recorded by gettext.
get_po_messages <- function(po_file) {
  if (!file.exists(po_file)) stop(domain=NA, gettextf("File not found: %s", po_file, domain="R-potools"))
  po_lines = readLines(po_file)
  po_length = length(po_lines)

  # number of msgstr corresponding to each msgid/msgid_plural pair depends on
  #   the language (e.g. just one for Chinese, but 3 for Polish). So we can't count
  #   any one of msgid, msgid_plural, or msgstr. For now, the max of msgid counts
  #   and msgstr counts sounds correct.
  # includes msgid_plural; anchor to ^ to skip fuzzied messages
  msgid_start = grep("^msgid", po_lines)
  msgstr_start = grep("^msgstr", po_lines)
  # pre-calculate which lines contain message continuations. Append
  #   FALSE for a while loop to terminate gracefully on hitting file end
  is_msg_continuation = c(grepl('^"', po_lines), FALSE)

  n_msg = max(length(msgid_start), length(msgstr_start))
  po_data = data.table(
    type = "singular",
    plural_index = NA_integer_,
    msgid = character(n_msg),
    msgstr = character(n_msg)
  )
  type = rep("singular", n_msg)
  msgid = character(length(msg_start))
  msgstr = character(length(msg_start))
  msg_j = 1L
  find_msg_end <- function(start_idx) {
    # returns the first FALSE found. since tail(.,1) is FALSE,
    #   guaranteed to give a match.
    # -start_idx includes skipping start_idx itself, so we won't
    #   end at the first line
    match(FALSE, tail(is_msg_continuation, -start_idx)) - 1L
  }
  build_msg <- function(x, tag) {
    paste(gsub(sprintf('^(?:%s )?"|"$', tag), '', x), collapse = '')
  }
  while (msg_j <= length(msg_start)) {
    start = msg_start[msg_j]
    end = find_msg_end(start)
    po_data[msg_j, msgid := build_msg(po_lines[start:end], 'msgid')]

    start = end + 1L
    if (grepl("^msgid_plural", po_lines[start])) {

    } else { # msgstr (not plural)
      end = find_msg_end(start)
      po_data[msg_j, msgstr := build_msg(po_lines[start:end], 'msgstr')]
      msg_j = msg_j + 1L
    }
  }
}
