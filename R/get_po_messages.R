# extract all the messages from a po file as a table matching the structure
#   found from get_r_messages, namely with 3 columns: type, plural_index, msgid.
#   two columns (file, call) are missing because (at least for R files)
#   these are not recorded by gettext.
get_po_messages <- function(po_file) {
  if (!file.exists(po_file)) stop(domain=NA, gettextf("File not found: %s", po_file, domain="R-potools"))
  po_lines = readLines(po_file)

  # includes msgid_plural; anchor to ^ to skip fuzzied messages
  msg_start = grep("^msgid", po_lines)
  n_msg = length(msg_start)
  po_data = data.table(
    type = "singular",
    plural_index = NA_integer_,
    msgid = character(n_msg),
    msgstr = character(n_msg)
  )
  type = rep("singular", n_msg)
  msgid = character(length(msg_start))
  msgstr = character(length(msg_start))
  for (msg_j in seq_along(msg_start)) {
    start = msg_start[msg_j]
    ii = start + 1L
    while (!grepl("^(?:msgstr|msgid_plural)", po_lines[ii])) {ii = ii+1L}
    msgid[msg_j] = paste(gsub('^(?:msgid )?"|"$', '', po_lines[msg_start:(ii-1L)]), collapse='')
    start = ii
  }
}
