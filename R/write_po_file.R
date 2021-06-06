# output R and/or src .po file(s) from a message data.table
# See https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files
# tag each msgid with where it's found in the source. messages that appear in
#   multiple places have each place tagged, space-separated. these are produced by
#   default by xgettext, etc (unless --no-location is set, or if --add-location=never).
# note also that the gettext manual says we shouldn't write these ourselves... for now i'm
#   going to go ahead and try to anyway until it breaks something :)
# TODO: respect xgettext --width= default? per gettext/gettext-tools/src/write-catalog.c,
#   the default is PAGE_WIDTH which is defined in gettext/gettext-tools/configure.ac as 79, which
#   matches what tools::update_pkg_po() produces for R-devel/src/library/base/po/R.pot, e.g.
#   note that xgettext is not run for R-*.pot files, so this width is not respected.
#   See also https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18121
# TODO: experiment with allowing source_location in R-*.pot? files. See #41.
write_po_files <- function(message_data, po_dir, params, template = FALSE) {
  # drop untranslated strings, collapse duplicates, drop unneeded data.
  #   for now, treating R & src separately so they can be treated differently; eventually this should
  #   be removed, or at least controlled by an option.
  # also considered:
  #   * rbind() each {R,src}x{singular,plural} combination together, but was getting quite lengthy/verbose/repetitive.
  #   * split(,by='message_source,type') but missing levels (e.g., src.plural) need to be handled separately
  po_data = message_data[(is_marked_for_translation)]

  if (template) {
    r_file <- sprintf("R-%s.pot", params$package)
    src_file <- sprintf("%s.pot", params$package)

    po_data[type == "plural", 'msgid_plural_str' := vapply(msgid_plural, paste, character(1L), collapse="|||")]
    po_data = po_data[,
      by = .(message_source, type, msgid, msgid_plural = msgid_plural_str),
      .(
        source_location = if (.BY$message_source == "R") "" else make_src_location(file, line_number),
        c_fmt_tag = if (grepl(SPRINTF_TEMPLATE_REGEX, .BY$msgid)) "#, c-format\n" else "",
        msgstr = if (.BY$type == 'singular') '' else NA_character_,
        msgstr_plural = if (.BY$type == "plural") list(c('', '')) else list(NULL)
      )
    ]
  } else {
    r_file <- sprintf("R-%s.po", params$language)
    src_file <- sprintf("%s.po", params$language)

    po_data[
      type == "plural",
      `:=`(
        msgid_plural_str = vapply(msgid_plural, paste, character(1L), collapse="|||"),
        msgstr_plural_str = vapply(msgstr_plural, paste, character(1L), collapse="|||")
      )
    ]
    po_data = po_data[,
      by = .(message_source, type, msgid, msgid_plural = msgid_plural_str),
      .(
        source_location = if (.BY$message_source == "R") "" else make_src_location(file, line_number),
        c_fmt_tag = if (grepl(SPRINTF_TEMPLATE_REGEX, .BY$msgid)) "#, c-format\n" else "",
        msgstr = msgstr[1L],
        # [1] should be a no-op here
        msgstr_plural = msgstr_plural_str[1L]
      )
    ]
    # only do in non-template branch b/c we can't define a dummy msgstr_plural that splits to list('', '')
    # don't filter to type=='plural' here -- causes a type conflict with the str elsewhere. we need a full plonk.
    po_data[ , 'msgstr_plural' := strsplit(msgstr_plural, "|||", fixed = TRUE)]
  }

  po_data[ , 'msgid_plural' := strsplit(msgid_plural, "|||", fixed = TRUE)]

  params$template = template
  write_po_file(
    po_data[message_source == "R"],
    file.path(po_dir, r_file),
    params
  )
  write_po_file(
    po_data[message_source == "src"],
    file.path(po_dir, src_file),
    params
  )
  return(invisible())
}

write_po_file <- function(message_data, po_file, params, template) {
  if (!nrow(message_data)) return(invisible())

  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  po_conn = file(po_file, "wb")
  on.exit(close(po_conn))

  params$has_plural = any(message_data$type == "plural")
  po_header = build_po_header(params, template)

  writeLines(con=po_conn, useBytes=TRUE, po_header)

  message_data[ , {
    out_lines = character(.N)
    singular_idx = type == 'singular'
    out_lines[singular_idx] = sprintf(
      '\n%s%smsgid "%s"\nmsgstr "%s"',
      source_location[singular_idx],
      c_fmt_tag[singular_idx],
      msgid[singular_idx],
      msgstr[singular_idx]
    )
    if (!all(singular_idx)) {
      msgid_plural = msgid_plural[!singular_idx]
      msgid1 = vapply(msgid_plural, `[`, character(1L), 1L)
      msgid2 = vapply(msgid_plural, `[`, character(1L), 2L)
      msgid_plural = vapply(
        msgstr_plural[!singular_idx],
        function(msgstr) paste(
          # TODO: should encodeString() be done directly at translation time?
          sprintf('msgstr[%d] "%s"', seq_along(msgstr)-1L, encodeString(msgstr)),
          collapse='\n'
        ),
        character(1L)
      )
      out_lines[!singular_idx] = sprintf(
        '\n%s%smsgid "%s"\nmsgid_plural "%s"\n%s',
        source_location[!singular_idx], c_fmt_tag[!singular_idx],
        msgid1, msgid2, msgid_plural
      )
    }

    writeLines(con=po_conn, useBytes=TRUE, out_lines)
  }]
}

build_po_header = function(params, template) {
  params$timestamp <- format(Sys.time(), tz = 'UTC')
  params$bugs <- if (is.null(params$bugs)) {
    sprintf("\nReport-Msgid-Bugs-To: %s\\n", params$bugs)
  } else {
    ''
  }

  if (params$template) {
    if (is.null(params$copyright)) {
      params$copyright_template <- NO_COPYRIGHT_TEMPLATE
      params$copyright <- ''
    } else {
      params$copyright_template <- with(params, sprintf(COPYRIGHT_TEMPLATE, copyright, package))
      params$copyright <- sprintf('\n"Copyright: %s\\n"', params$copyright)
    }
    params$po_revision_date <- 'YEAR-MO-DA HO:MI+ZONE'
    params$author <- 'FULL NAME <EMAIL@ADDRESS>'
    params$lang_team <- 'LANGUAGE <LL@li.org>'
    params$lang_name <- ''
    params$charset <- "CHARSET"
    params$plural_forms <- if (params$has_plural) {
      '\n"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n"'
    } else {
      ''
    }
  } else {
    # TODO(#76): don't do this
    params$copyright_template <- NO_COPYRIGHT_TEMPLATE
    if (is.null(params$copyright)) {
      params$copyright <- ''
    } else {
      params$copyright <- sprintf('\n"Copyright: %s\\n"', params$copyright)
    }
    params$po_revision_date <- params$timestamp
    params$lang_team <- params$lang_team <- params$full_name_eng
    params$charset <- "UTF-8"
    params$plural_forms <- if (params$has_plural) {
      with(params, sprintf('\n"Plural-Forms: nplurals=%d; plural=%s;\\n"', nplural, plural))
    } else {
      ''
    }
  }

  with(params, sprintf(
    PO_HEADER_TEMPLATE,
    copyright_template,
    package, version,
    bugs,
    timestamp,
    po_revision_date,
    author,
    lang_team,
    lang_name,
    copyright,
    charset,
    plural_forms
  ))
}

# see circa lines 2036-2046 of gettext/gettext-tools/src/xgettext.c
COPYRIGHT_TEMPLATE = 'SOME DESCRIPTIVE TITLE.
Copyright (C) YEAR %s
This file is distributed under the same license as the %s package.
FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
'
NO_COPYRIGHT_TEMPLATE = 'SOME DESCRIPTIVE TITLE.
This file is put in the public domain.
FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
'

# balance here: keeping newlines in the string to facilitate writing,
#   but need to escape the in-string newlines or they'll be written
#   as newlines (not literal \n). encodeString is "soft-applied" here.
#   might be better to treat this as a DCF and write it from a list
#   instead of building it up from sprintf
PO_HEADER_TEMPLATE = '%smsgid ""
msgstr ""
"Project-Id-Version: %s %s\\n"%s
"POT-Creation-Date: %s\\n"
"PO-Revision-Date: %s\\n"
"Last-Translator: %s\\n"
"Language-Team: %s\\n"
"Language: %s\\n"%s
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=%s\\n"
"Content-Transfer-Encoding: 8bit\\n%s'

make_src_location <- function(files, lines) {
  # NB: technically basename() is incorrect since relative paths are made, but I'm not
  #   sure how the top-level path is decided for this. must be fixed to handle base.
  paste0("#: ", paste(sprintf("%s:%d", basename(files), lines), collapse = " "), "\n")
}
