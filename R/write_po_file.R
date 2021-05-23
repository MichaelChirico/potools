# output R and/or src .po file(s) from a message data.table
write_po_files <- function(message_data, po_dir, language, package, version, author, metadata) {
  timestamp <- format(Sys.time(), tz = 'UTC')

  po_header <- sprintf(
    PO_HEADER_TEMPLATE,
    package, version,
    timestamp,
    timestamp,
    author,
    metadata$full_name_eng,
    metadata$full_name_eng,
    metadata$nplurals, metadata$plural
  )

  write_po_file(message_data[message_source == "R"], file.path(po_dir, sprintf("R-%s.po", language)), po_header)
  write_po_file(message_data[message_source == "src"], file.path(po_dir, sprintf("%s.po", language)), po_header)
  return(invisible())
}

write_po_file <- function(message_data, po_file, poheader) {
  if (!nrow(message_data)) return(invisible())

  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  po_conn = file(po_file, "wb")
  on.exit(close(po_conn))

  writeLines(con=po_conn, useBytes=TRUE, po_header)

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
