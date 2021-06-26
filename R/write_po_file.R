# output R and/or src .po file(s) from a message data.table
# See https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files
# tag each msgid with where it's found in the source. messages that appear in
#   multiple places have each place tagged, space-separated. these are produced by
#   default by xgettext, etc (unless --no-location is set, or if --add-location=never).
# note also that the gettext manual says we shouldn't write these ourselves... for now i'm
#   going to go ahead and try to anyway until it breaks something :)
write_po_files <- function(message_data, po_dir, params, template = FALSE, use_base_rules = FALSE) {
  # drop untranslated strings, collapse duplicates, drop unneeded data.
  #   for now, treating R & src separately so they can be treated differently; eventually this should
  #   be removed, or at least controlled by an option.
  # also considered:
  #   * rbind() each {R,src}x{singular,plural} combination together, but was getting quite lengthy/verbose/repetitive.
  #   * split(,by='message_source,type') but missing levels (e.g., src.plural) need to be handled separately
  # also drop empty strings. these are kept until now in case they are needed for diagnostics, but can't be
  #   written to the .po/.pot files (msgid "" is reserved for the metadata header). Related: #83.
  po_data = message_data[is_marked_for_translation & (type == "plural" | nzchar(msgid, keepNA = TRUE))]

  if (template) {
    r_file <- sprintf("R-%s.pot", params$package)
    src_file <- sprintf("%s.pot", if (params$package == 'base') 'R' else params$package)

    po_data[type == "plural", 'msgid_plural_str' := vapply(msgid_plural, paste, character(1L), collapse="|||")]
    po_data = po_data[,
      by = .(message_source, type, msgid, msgid_plural = msgid_plural_str),
      .(
        source_location = make_src_location(file, line_number, .BY$message_source, use_base_rules),
        c_fmt_tag = "",
        msgstr = if (.BY$type == 'singular') '' else NA_character_,
        msgstr_plural = if (.BY$type == "plural") list(c('', '')) else list(NULL),
        # see discussion in #137
        is_templated = any(is_templated)
      )
    ]
    if (use_base_rules) {
      po_data[message_source == 'src' & is_templated, 'c_fmt_tag' := "#, c-format\n"]
    } else {
      po_data[(is_templated), 'c_fmt_tag' := "#, c-format\n"]
    }
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
        source_location = make_src_location(file, line_number, .BY$message_source, use_base_rules),
        c_fmt_tag = "",
        msgstr = msgstr[1L],
        # [1] should be a no-op here
        msgstr_plural = msgstr_plural_str[1L],
        is_templated = any(is_templated)
      )
    ]
    if (use_base_rules) {
      po_data[message_source == 'src' & is_templated, 'c_fmt_tag' := "#, c-format\n"]
    } else {
      po_data[(is_templated), 'c_fmt_tag' := "#, c-format\n"]
    }
    # only do in non-template branch b/c we can't define a dummy msgstr_plural that splits to list('', '')
    # don't filter to type=='plural' here -- causes a type conflict with the str elsewhere. we need a full plonk.
    po_data[ , 'msgstr_plural' := strsplit(msgstr_plural, "|||", fixed = TRUE)]
  }

  po_data[ , 'msgid_plural' := strsplit(msgid_plural, "|||", fixed = TRUE)]

  params$base_copyright <- FALSE
  params$is_base_package <- params$package %chin% .potools$base_package_names
  if (params$is_base_package) {
    params$package <- "R"
    params$bugs <- "bugs.r-project.org"
  }

  params$template = template
  params$ignore_width = use_base_rules
  write_po_file(
    po_data[message_source == "R"],
    file.path(po_dir, r_file),
    params,
    use_base_rules = use_base_rules
  )
  # assign here to prevent lazyeval issue
  width = if (use_base_rules) 79L else Inf
  # See #125 and Bugzilla#18121. suggestions for a less horrible workaround welcome.
  params$ignore_width = FALSE
  # only applies to src .pot (part of https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18121)
  if (params$is_base_package) {
    params$copyright <- "The R Core Team"
    params$base_copyright <- TRUE
  }
  write_po_file(
    po_data[message_source == "src"],
    file.path(po_dir, src_file),
    params,
    width = width,
  )
  return(invisible())
}

write_po_file <- function(message_data, po_file, params, width = Inf, use_base_rules = FALSE) {
  if (!nrow(message_data)) return(invisible())

  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  po_conn = file(po_file, "wb")
  on.exit(close(po_conn))

  params$has_plural = any(message_data$type == "plural")
  po_header = build_po_header(params, use_base_rules)

  writeLines(con=po_conn, useBytes=TRUE, po_header)

  if (use_base_rules) {
    plural_fmt <- '\n%s%smsgid        "%s"\nmsgid_plural "%s"\n%s'
    msgstr_fmt <- 'msgstr[%d]    "%s"'

  } else {
    plural_fmt <- '\n%s%smsgid "%s"\nmsgid_plural "%s"\n%s'
    msgstr_fmt <- 'msgstr[%d] "%s"'
  }
  message_data[ , {
    out_lines = character(.N)
    singular_idx = type == 'singular'
    out_lines[singular_idx] = sprintf(
      '\n%s%s%s\n%s',
      source_location[singular_idx],
      c_fmt_tag[singular_idx],
      wrap_msg('msgid', msgid[singular_idx], width, params$ignore_width),
      wrap_msg('msgstr', msgstr[singular_idx], width, params$ignore_width)
    )
    if (!all(singular_idx)) {
      msgid_plural = msgid_plural[!singular_idx]
      msgid1 = vapply(msgid_plural, `[`, character(1L), 1L)
      msgid2 = vapply(msgid_plural, `[`, character(1L), 2L)
      msgid_plural = vapply(
        msgstr_plural[!singular_idx],
        function(msgstr) paste(
          sprintf(msgstr_fmt, seq_along(msgstr)-1L, msgstr),
          collapse='\n'
        ),
        character(1L)
      )
      out_lines[!singular_idx] = sprintf(
        plural_fmt,
        source_location[!singular_idx],
        c_fmt_tag[!singular_idx],
        msgid1, msgid2, msgid_plural
      )
    }

    writeLines(con=po_conn, useBytes=TRUE, out_lines)
  }]
}

build_po_header = function(params, use_base_rules = FALSE) {
  params$timestamp <- format(Sys.time(), tz = 'UTC')

  if (is.null(params$bugs)) {
    params$bugs <- ''
  } else {
    params$bugs <- sprintf('\n"Report-Msgid-Bugs-To: %s\\n"', params$bugs)
  }

  if (params$template) {
    # TODO: this is confusing... revisit?
    if (params$base_copyright) {
      params$copyright_template <- with(params, sprintf(COPYRIGHT_TEMPLATE, copyright, package))
      params$copyright <- ''
      params$fuzzy_header <- "#, fuzzy\n"
    } else if (use_base_rules) {
      params$copyright_template <- params$copyright <- params$fuzzy_header <- ''
    } else if (is.null(params$copyright)) {
      params$copyright_template <- NO_COPYRIGHT_TEMPLATE
      params$copyright <- ''
      params$fuzzy_header <- "#, fuzzy\n"
    } else {
      params$copyright_template <- with(params, sprintf(COPYRIGHT_TEMPLATE, copyright, package))
      params$copyright <- sprintf('\n"Copyright: %s\\n"', params$copyright)
      params$fuzzy_header <- "#, fuzzy\n"
    }
    params$po_revision_date <- 'YEAR-MO-DA HO:MI+ZONE'
    params$author <- 'FULL NAME <EMAIL@ADDRESS>'
    params$lang_team <- 'LANGUAGE <LL@li.org>'
    params$lang_name <- if (use_base_rules) '' else '\n"Language: \\n"'
    params$charset <- "CHARSET"
    params$plural_forms <- if (!use_base_rules && params$has_plural) {
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
    # get a warning from msgfmt: PO file header fuzzy; older versions of msgfmt will give an error on this
    params$fuzzy_header <- ''
    params$po_revision_date <- params$timestamp
    params$lang_team <- params$full_name_eng
    # must write Language: in the .po file
    params$lang_name <- sprintf('\n"Language: %s\\n"', params$lang_team)
    params$charset <- "UTF-8"
    params$plural_forms <- if (params$has_plural) {
      with(params, sprintf('\n"Plural-Forms: nplurals=%s; plural=%s;\\n"', nplurals, plural))
    } else {
      ''
    }
  }

  with(params, sprintf(
    PO_HEADER_TEMPLATE,
    copyright_template, fuzzy_header,
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

wrap_msg = function(key, value, width, ignore_width = FALSE) {
  out <- character(length(value))
  # xgettext always wraps at a newline (even if the whole message fits inside 'width')
  if (ignore_width) {
    wrap_idx <- rep(FALSE, length(value))
  } else {
    wrap_idx <- nchar(value) + nchar(key) + 3L > width | grepl("[\\]n.", value)
  }
  out[!wrap_idx] = sprintf('%s "%s"', key, value[!wrap_idx])
  out[wrap_idx] = sprintf('%s ""\n%s', key, wrap_strings(value[wrap_idx], width))
  out
}

# strwrap gets oh-so-close. but the xgettext behavior splits at more characters (e.g. [.]);
#   so we roll our own
wrap_strings = function(str, width) {
  if (!length(str)) return(character())

  boundaries = gregexpr(XGETTEXT_BOUNDARY_REGEX, str, perl = TRUE)
  # xgettext _doesn't_ break on escaped-backslash-then-n, so match to an odd number of backslashes-then-n
  # append . to simplify the logic below (and besides, the string won't ever split after the very end anyway)
  has_newlines = grepl('(?:^|[^\\])[\\](?:[\\][\\]){0,}n.', str)
  str_widths = nchar(str)

  out = character(length(str))
  for (ii in seq_along(str)) {
    if (has_newlines[ii]) {
      # split the string at newlines, then wrap each "segment" as we would other msgid.
      #   eschew strsplit to do the splitting because it would require lookahead/lookbehind, which means
      #   perl, which means a monstrosity w.r.t. escaped backslashes.
      # TODO: feels like disastrously bad code. Would it be easier to always just iterate by word?
      newline_indices = gregexpr('(?:^|[^\\])[\\](?:[\\][\\]){0,}n.', str[ii])[[1L]]
      # +2 to adjust for -2 below
      newline_indices = c(0L, newline_indices + attr(newline_indices, "match.length") - 2L, str_widths[ii] + 2L)
      sub_str = character(length(newline_indices) - 1L)
      for (jj in seq_along(sub_str)) {
        # -2 to exclude \n
        sub_str[jj] = substr(str[ii], newline_indices[jj]+1L, newline_indices[jj+1L]-2L)
      }
      sub_boundaries = gregexpr('[ !,-./:;?|}](?![ !,-./:;?|}])|[^\'](?=\'?%)', sub_str, perl = TRUE)
      sub_str_widths = nchar(sub_str)
      lines = vapply(
        seq_along(sub_str),
        function(jj) wrap_string(sub_str[jj], sub_boundaries[[jj]], sub_str_widths[jj], width),
        character(1L)
      )
      # stitch sub_str components by re-appending the \n, _inside_ the " arrays, then add the outer-outer " to finish
      out[ii] = paste0('"', paste(lines, collapse = '\\n"\n"'), '"')
    } else {
      out[ii] = paste0('"', wrap_string(str[ii], boundaries[[ii]], str_widths[ii], width), '"')
    }
  }
  out
}

# valid splits for xgettext found by experimentation (couldn't find where in the source this is defined).
#   write _("abcdefghijklm${CHAR}nopqrtstuvwxyz") for these ASCII $CHARs:
#   rawToChar(as.raw(c(32:33, 35:47, 58:64, 91, 93:96, 123:126)))
#   then run the following to find which lines were split at the character before 'n':
#   xgettext --keyword=_ --width=20 $TMPFILE -o /dev/stdout | grep -F '"n' -B 1
# more experimentation shows
#   - a preference to put formatting % on the next line too
#      + including "dragging" certain surrounding characters along, e.g. `-`, '`, `[`, `|`
#   - pick the lattermost line splitter when they come consecutively
#   - \" is considered a boundary _if not preceded by [0-9()']_, see #91
# Some insights on the source: x-c.c is the lexer that does preprocessing:
#   https://cvs.savannah.gnu.org/viewvc/gettext/gettext/gettext-tools/src/x-c.c?view=markup
XGETTEXT_BOUNDARY_REGEX <- paste(
  '[ !,-./:;?|}](?![ !,-./:;?|}])',
  '[^-\'\\[|](?=[-\'\\[|]?%)',
  '[^0-9()\'](?=[\\\\]")',
  sep = '|'
)

wrap_string = function(str, boundary, str_width, line_width) {
  # no places to split this string, so don't. xgettext also seems not to.
  if (boundary[1L] < 0L) return(str)

  # supplement with the total string width for the case that the last word breaks the width
  boundary = c(boundary, str_width)
  lines = character()

  # 0 not 1 makes the arithmetic nicer below
  start_char = 0L
  # 2 accounts for two " (added below)
  while (any(wide_idx <- boundary > line_width - 2L)) {
    split_idx = max(which(wide_idx)[1L] - 1L, 1L)
    lines = c(lines, substr(str, start_char + 1L, start_char + boundary[split_idx]))
    start_char = start_char + boundary[split_idx]
    boundary = tail(boundary, -split_idx) - boundary[split_idx]
  }
  if (start_char < str_width) lines = c(lines, substr(str, start_char + 1L, str_width))

  # wrap only internally here -- for the newline-broken case, we need to build the "outer" wrapper idiosyncratically
  paste(lines, collapse = '"\n"')
}

# see circa lines 2036-2046 of gettext/gettext-tools/src/xgettext.c
COPYRIGHT_TEMPLATE = '# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR %s
# This file is distributed under the same license as the %s package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
'
NO_COPYRIGHT_TEMPLATE = '# SOME DESCRIPTIVE TITLE.
# This file is put in the public domain.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
'

# balance here: keeping newlines in the string to facilitate writing,
#   but need to escape the in-string newlines or they'll be written
#   as newlines (not literal \n). encodeString is "soft-applied" here.
#   might be better to treat this as a DCF and write it from a list
#   instead of building it up from sprintf
PO_HEADER_TEMPLATE = '%s%smsgid ""
msgstr ""
"Project-Id-Version: %s %s\\n"%s
"POT-Creation-Date: %s\\n"
"PO-Revision-Date: %s\\n"
"Last-Translator: %s\\n"
"Language-Team: %s\\n"%s%s
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=%s\\n"
"Content-Transfer-Encoding: 8bit\\n"%s'

make_src_location <- function(files, lines, message_source, use_base_rules) {
  if (use_base_rules && message_source == "R") return("")
  s <- paste(sprintf("%s:%d", files, lines), collapse = " ")
  # branch above implies use_base_rules => message_source == "src"
  # 77 = 80 - nchar("#: "). 80 not 79 is for strwrap. NB: strwrap("012 345", width=4)
  paste0("#: ", if (use_base_rules) strwrap(s, width=77L) else s, "\n", collapse="")
}
