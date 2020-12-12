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

  for (ii in seq_len(nrow(message_data))) {
    message_data[ii, {
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

# TODO: apparently not working:
#   R-zh_CN:po/R-zh_CN.po:4: end-of-line within string
# po/R-zh_CN.po:5: end-of-line within string
# po/R-zh_CN.po:6: end-of-line within string
# po/R-zh_CN.po:7: end-of-line within string
# po/R-zh_CN.po:8: end-of-line within string
# po/R-zh_CN.po:9: end-of-line within string
# po/R-zh_CN.po:10: end-of-line within string
# po/R-zh_CN.po:11: end-of-line within string
# po/R-zh_CN.po:12: end-of-line within string
# po/R-zh_CN.po:13: end-of-line within string
# po/R-zh_CN.po:14: end-of-line within string
# po/R-zh_CN.po:15: end-of-line within string
# po/R-zh_CN.po:16: end-of-line within string
# po/R-zh_CN.po:17: end-of-line within string
# po/R-zh_CN.po:18: end-of-line within string
# po/R-zh_CN.po:19: end-of-line within string
# po/R-zh_CN.po:20: end-of-line within string
# po/R-zh_CN.po:21: end-of-line within string
# po/R-zh_CN.po:22: end-of-line within string
# po/R-zh_CN.po:23: end-of-line within string
# po/R-zh_CN.po: warning: Charset "UTF-8Content-Transfer-Encoding:" is not a portable encoding name.
#                         Message conversion to user's charset might not work.
# po/R-zh_CN.po:58: missing 'msgstr' section
# po/R-zh_CN.po:58:42: syntax error
# po/R-zh_CN.po:58: keyword "s" unknown
# po/R-zh_CN.po:59: end-of-line within string
# po/R-zh_CN.po:59: keyword "Valid" unknown
# po/R-zh_CN.po:59: keyword "fields" unknown
# po/R-zh_CN.po:59: keyword "are" unknown
# po/R-zh_CN.po:59: keyword "s" unknown
# po/R-zh_CN.po:60: end-of-line within string
# po/R-zh_CN.po:60: keyword "d" unknown
# po/R-zh_CN.po:60: keyword "s" unknown
# po/R-zh_CN.po:69:11: syntax error
# po/R-zh_CN.po:69: keyword "$d" unknown
# po/R-zh_CN.po:69: keyword "d" unknown

