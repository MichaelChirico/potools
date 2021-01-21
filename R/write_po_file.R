# output a .po file from a message data.table
write_po_file <- function(message_data, pofile, package, version, author, metadata) {
  timestamp <- format(Sys.time(), tz = 'UTC')
  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  pofile_conn = file(pofile, "wb")
  on.exit(close(pofile_conn))

  writeLines(con=pofile_conn, useBytes=TRUE, sprintf(
    PO_HEADER_TEMPLATE,
    package, version,
    timestamp,
    timestamp,
    author,
    metadata$full_name_eng,
    metadata$full_name_eng,
    metadata$nplurals, metadata$plural
  ))

  for (ii in message_data[(!is_repeat), which = TRUE]) {
    message_data[ii, {
      if (type == 'singular') {
        writeLines(con=pofile_conn, useBytes=TRUE, sprintf(
          '\n\nmsgid "%s"\nmsgstr "%s"', msgid, msgstr
        ))
      } else {
        # stored in list column, we need to [[1]]
        msgstr = plural_msgstr[[1L]]
        writeLines(con=pofile_conn, useBytes=TRUE, sprintf(
          '\n\nmsgid "%s"\nmsgid_plural "%s"\n%s',
          plural_msgid[[c(1L, 1L)]], plural_msgid[[1:2]],
          paste(sprintf('msgstr[%d] "%s"', seq_along(msgstr)-1L, msgstr), collapse='\n')
        ))
      }
    }]
  }
}

# balance here: keeping newlines in the string to facilitate writing,
#   but need to escape the in-string newlines or they'll be written
#   as newlines (not literal \n). encodeString is "soft-applied" here.
#   might be better to tread this as a DCF and write it from a list
#   instead of building it up from sprintf
PO_HEADER_TEMPLATE = 'msgid ""
msgstr ""
"Project-Id-Version: %s %s\\n"
"POT-Creation-Date: %s\\n"
"PO-Revision-Date: %s\\n"
"Last-Translator: %s\\n"
"Language-Team: %s\\n"
"Language: %s\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"Plural-Forms: nplurals=%d; plural=%s;\\n"
'
