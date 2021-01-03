# output a .po file from a message data.table
write_po_file <- function(message_data, pofile, package, version, author, metadata) {
  timestamp <- format(Sys.time(), tz = 'UTC')
  cat(file=pofile, sprintf(
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
      cat(sprintf("msgid=%s\tmsgstr=%s\n", msgid, msgstr))
      if (type == 'singular') {
        cat(sprintf('\n\nmsgid "%s"\nmsgstr "%s"', msgid, msgstr), file=pofile, append=TRUE)
      } else {
        cat(file=pofile, append=TRUE, sprintf(
          '\n\nmsgid "%s"\nmsgid_plural "%s"\n%s',
          plural_msgid[[c(1L, 1L)]], plural_msgid[[1:2]],
          paste(sprintf('msgstr[%d] "%s"', seq_along(plural_msgstr)-1L, unlist(plural_msgstr)), collapse='\n')
        ))
      }
    }]
  }
  cat('\n', file=pofile, append=TRUE)
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
