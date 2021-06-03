# output R and/or src .po file(s) from a message data.table
# TODO: API here is a mess. don't have the heart to refactor right now though.
write_po_files <- function(message_data, po_dir, language, package, version, author, metadata, template = FALSE) {
  timestamp <- format(Sys.time(), tz = 'UTC')

  if (template) {
    po_revision_date <- 'YEAR-MO-DA HO:MI+ZONE'
    author <- 'FULL NAME <EMAIL@ADDRESS>'
    lang_team <- 'LANGUAGE <LL@li.org>'
    lang_name <- ''
    plural_forms <- ''
    charset <- "CHARSET"
  } else {
    po_revision_date <- timestamp
    lang_team <- metadata$full_name_eng
    lang_name <- sprintf("\nLanguage: %s\\n", lang_team)
    plural_forms <- sprintf(
      '"Plural-Forms: nplurals=%d; plural=%s;\\n"\n',
      metadata$nplurals, metadata$plural
    )
    charset <- "UTF-8"
  }
  po_header <- sprintf(
    PO_HEADER_TEMPLATE,
    package, version,
    timestamp,
    po_revision_date,
    author,
    lang_team,
    lang_name,
    charset,
    plural_forms
  )

  r_file <- if (template) sprintf("R-%s.pot", package) else sprintf("R-%s.po", language)
  write_po_file(
    message_data[message_source == "R"],
    file.path(po_dir, r_file),
    po_header,
    template = template
  )
  src_file <- if (template) sprintf("%s.pot", package) else sprintf("%s.po", language)
  write_po_file(
    message_data[message_source == "src" & is_marked_for_translation],
    file.path(po_dir, src_file),
    po_header,
    template = template
  )
  return(invisible())
}

# a small wrapper
write_pot_files <- function(message_data, po_dir, package, version) {
  write_po_files(
    message_data, po_dir,
    package = package,
    version = version,
    template = TRUE
  )
}

write_po_file <- function(message_data, po_file, po_header, template = FALSE) {
  if (!nrow(message_data)) return(invisible())

  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  po_conn = file(po_file, "wb")
  on.exit(close(po_conn))

  writeLines(con=po_conn, useBytes=TRUE, po_header)

  message_data[(!is_repeat), {
    out_lines = character(.N)
    singular_idx = type == 'singular'
    out_lines[singular_idx] = sprintf(
      '\nmsgid "%s"\nmsgstr "%s"',
      msgid[singular_idx],
      if (template) "" else msgstr[singular_idx]
    )
    if (!all(singular_idx)) {
      msgid_plural = msgid_plural[!singular_idx]
      msgid1 = vapply(msgid_plural, `[`, character(1L), 1L)
      msgid2 = vapply(msgid_plural, `[`, character(1L), 2L)
      if (template) {
        msgid_plural = 'msgstr[0] ""\nmsgstr[1] ""'
      } else {
        msgid_plural = vapply(
          msgstr_plural[!singular_idx],
          function(msgstr) paste(
            # TODO: should encodeString() be done directly at translation time?
            sprintf('msgstr[%d] "%s"', seq_along(msgstr)-1L, encodeString(msgstr)),
            collapse='\n'
          ),
          character(1L)
        )
      }
      out_lines[!singular_idx] = sprintf(
        '\nmsgid "%s"\nmsgid_plural "%s"\n%s',
        msgid1, msgid2, msgid_plural
      )
    }

    writeLines(con=po_conn, useBytes=TRUE, out_lines)
  }]
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
"Language-Team: %s\\n"%s
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=%s\\n"
"Content-Transfer-Encoding: 8bit\\n"
%s'
