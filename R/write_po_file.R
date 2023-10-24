# output R and/or src .po file(s) from a message data.table
# See https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files
# tag each msgid with where it's found in the source. messages that appear in
#   multiple places have each place tagged, space-separated. these are produced by
#   default by xgettext, etc (unless --no-location is set, or if --add-location=never).
# note also that the gettext manual says we shouldn't write these ourselves... for now i'm
#   going to go ahead and try to anyway until it breaks something :)
write_po_files <- function(message_data, po_dir, params, template = FALSE, use_base_rules = FALSE, verbose = TRUE) {
  if (template) {
    r_file <- glue_data(params, "R-{package}.pot")
    src_file <- glue_data(params, "{if (package == 'base') 'R' else package}.pot")
  } else {
    r_file <- glue_data(params, "R-{language}.po")
    src_file <- glue_data(params, "{language}.po")
  }

  is_base_package <- params$package %chin% .potools$base_package_names
  if (is_base_package) {
    params$package <- "R"
    params$bugs <- "bugs.r-project.org"
    # #299: msg* commands work best with charset=UTF-8,
    #   but the 'base' .pot files leave it unsepecified (CHARSET).
    if (template) params$charset <- "CHARSET"
  }

  if (template) {
    metadata = with(params, po_metadata(
      package = package, version = version,
      bugs = bugs, copyright = copyright
    ))
  } else {
    metadata = with(params, po_metadata(
      package = package, version = version, language = language,
      author = author, email = email, bugs = bugs, copyright = copyright,
      `X-Generator` = glue("potools {packageVersion('potools')}")
    ))
  }

  if (verbose) messagef('Writing %s', r_file)

  write_po_file(
    message_data[message_source == "R"],
    file.path(po_dir, r_file),
    metadata,
    width = if (use_base_rules) Inf else 79L,
    wrap_at_newline = !use_base_rules,
    use_base_rules = use_base_rules
  )
  # only applies to src .pot (part of https://bugs.r-project.org/bugzilla/show_bug.cgi?id=18121)
  if (is_base_package) {
    metadata$copyright$holder <- "The R Core Team"
  }
  write_po_file(
    message_data[message_source == "src"],
    file.path(po_dir, src_file),
    metadata,
    use_base_rules = use_base_rules
  )
  return(invisible())
}



#' Write a .po or .pot file corresponding to a message database
#'
#' Serialize a message database in the \file{.po} and \file{.pot} formats
#' recognized by the gettext ecosystem.
#'
#' Three components are set automatically if not provided:
#'
#' * `pot_timestamp` - A `POSIXct` used to write the
#'   `POT-Creation-Date` entry. Defaults to the [Sys.time()] at
#'   run time.
#' * `po_timestamp` - A `POSIXct` used to write the
#'   `PO-Revision-Date` entry. Defaults to be the same as
#'   `pot_timestamp`.
#' *  `language_team` - A string used to write
#'    the `Language-Team` entry. Defaults to be the same as `language`;
#'    if provided manually, the format `LANGUAGE <LL@li.org>` is recommended.
#'
#' The `charset` for output is always set to `"UTF-8"`; this is
#' intentional to make it more cumbersome to create non-UTF-8 files.
#'
#' @aliases write_po_file po_metadata format.po_metadata print.po_metadata
#' @param message_data `data.table`, as returned from
#' [get_message_data()]. NB: R creates separate domains for R and
#' C/C++ code; it is recommended you do the same by filtering the
#' `get_message_data` output for `message_source == "R"` or
#' `message_source == "src"`. Other approaches are untested.
#' @param po_file Character vector giving a destination path. Paths ending in
#' \file{.pot} will be written with template files (e.g., `msgstr` entries
#' will be blanked).
#' @param metadata A `po_metadata` object as returned by
#' `po_metadata()`.
#' @param width Numeric governing the wrapping width of the output file.
#' Default is `79L` to match the behavior of the `xgettext` utility.
#' `Inf` turns off wrapping (except for file source markers comments).
#' @param wrap_at_newline Logical, default `TRUE` to match the
#' `xgettext` utility's behavior. If `TRUE`, any `msgid` or
#' `msgstr` will always be wrapped at an internal newline (i.e., literally
#' matching `\n`).
#' @param use_base_rules Logical; Should internal behavior match base behavior
#' as strictly as possible? `TRUE` if being run on a base package (i.e.,
#' `base` or one of the default packages like `utils`,
#' `graphics`, etc.). See Details.
#' @param package Character; the name of the package being translated.
#' @param version Character; the version of the package being translated.
#' @param language Character; the language of the `msgstr`. See
#' [translate_package()] for details.
#' @param author Character; an author (combined with `email`) to whom to
#' attribute the translations (as `Last-Translator`).
#' @param email Character; an e-mail address associated with `author`.
#' @param bugs Character; a URL where issues with the translations can be
#' reported.
#' @param copyright An object used to construct the initial Copyright reference
#' in the output. If `NULL`, no such comment is written. If a `list`,
#' it should the following structure:
#'
#'   * `year`: Required, A year or hyphen-separated range of years
#'   * `holder`: Required, The name of the copyright holder
#'   * `title`: Optional, A title for the \file{.po}
#'   * `additional`: Optional, A character vector of additional lines for the
#'      copyright comment section
#'
#' If a `character` scalar, it is interpreted as the `holder` and the `year`
#' is set as the `POT-Creation-Date`'s year.
#' @param ...  Additional (named) components to add to the metadata. For
#' `print.po_metadata`, passed on to `format.po_metadata`
#' @param x A `po_metadata` object.
#' @param template Logical; format the metadata as in a \file{.pot} template?
#' @param use_plurals Logical; should the `Plural-Forms` entry be
#' included?
#' @return For `po_metadata`, an object of class `po_metadata` that
#' has a `format` method used to serialize the metadata.
#' @author Michael Chirico
#' @seealso [translate_package()], [get_message_data()],
#' [tools::xgettext2pot()], [tools::update_pkg_po()]
#' @references
#' <https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html>
#' \cr
#' @examples
#'
#' message_data <- get_message_data(system.file('pkg', package='potools'))
#' desc_data <- read.dcf(system.file('pkg', 'DESCRIPTION', package='potools'), c('Package', 'Version'))
#' metadata <- po_metadata(
#'   package = desc_data[, "Package"], version = desc_data[, "Version"],
#'   language = 'ar_SY', author = 'R User', email = 'ruser@gmail.com',
#'   bugs = 'https://github.com/ruser/potoolsExample/issues'
#' )
#'
#' # add fake translations
#' message_data[type == "singular", msgstr := "<arabic translation>"]
#' # Arabic has 6 plural forms
#' message_data[type == "plural", msgstr_plural := .(as.list(sprintf("<%d translation>", 0:5)))]
#'
#' # Preview metadata
#' print(metadata)
#' # write .po file
#' write_po_file(
#'   message_data[message_source == "R"],
#'   tmp_po <- tempfile(fileext = '.po'),
#'   metadata
#' )
#' writeLines(readLines(tmp_po))
#'
#' # write .pot template
#' write_po_file(
#'   message_data[message_source == "R"],
#'   tmp_pot <- tempfile(fileext = '.pot'),
#'   metadata
#' )
#' writeLines(readLines(tmp_pot))
#'
#' # cleanup
#' file.remove(tmp_po, tmp_pot)
#' rm(message_data, desc_data, metadata, tmp_po, tmp_pot)
#' @export
write_po_file <- function(
  message_data, po_file, metadata,
  width = 79L, wrap_at_newline = TRUE,
  use_base_rules = metadata$package %chin% .potools$base_package_names
) {
  if (!nrow(message_data)) return(invisible())

  template = endsWith(po_file, ".pot")

  # cat seems to fail at writing UTF-8 on Windows; useBytes should do the trick instead:
  #   https://stackoverflow.com/q/10675360
  po_conn = file(po_file, "wb")
  on.exit(close(po_conn))

  po_header = format(
    metadata,
    template = template,
    use_plurals = any(message_data$type == "plural")
  )

  writeLines(con=po_conn, useBytes=TRUE, po_header)

  # drop untranslated strings, collapse duplicates, drop unneeded data.
  #   for now, treating R & src separately so they can be treated differently; eventually this should
  #   be removed, or at least controlled by an option.
  # also considered:
  #   * rbind() each {R,src}x{singular,plural} combination together, but was getting quite lengthy/verbose/repetitive.
  #     also won't work for src because plural messages are interwoven there, not tucked at the end.
  #   * split(,by='message_source,type') but missing levels (e.g., src.plural) need to be handled separately
  # also drop empty strings. these are kept until now in case they are needed for diagnostics, but can't be
  #   written to the .po/.pot files (msgid "" is reserved for the metadata header). Related: #83.
  po_data = message_data[is_marked_for_translation & (type == "plural" | nzchar(msgid, keepNA = TRUE))]
  if (template) {
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
  } else {
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
    # only do in non-template branch b/c we can't define a dummy msgstr_plural that splits to list('', '')
    # don't filter to type=='plural' here -- causes a type conflict with the str elsewhere. we need a full plonk.
    po_data[ , 'msgstr_plural' := strsplit(msgstr_plural, "|||", fixed = TRUE)]
  }
  if (use_base_rules) {
    po_data[message_source == 'src' & is_templated, 'c_fmt_tag' := "#, c-format\n"]
  } else {
    po_data[(is_templated), 'c_fmt_tag' := "#, c-format\n"]
  }
  po_data[ , 'msgid_plural' := strsplit(msgid_plural, "|||", fixed = TRUE)]

  # tools::xgettext2pot() tries to make the entries' whitespace align, which xgettext doesn't do
  if (use_base_rules && po_data$message_source[1L] == "R") {
    plural_fmt <- '\n%s%smsgid        "%s"\nmsgid_plural "%s"\n%s'
    msgstr_fmt <- 'msgstr[%d]    "%s"'
  } else {
    plural_fmt <- '\n%s%smsgid "%s"\nmsgid_plural "%s"\n%s'
    msgstr_fmt <- 'msgstr[%d] "%s"'
  }

  po_data[ , {
    out_lines = character(.N)
    singular_idx = type == 'singular'
    out_lines[singular_idx] = sprintf(
      '\n%s%s%s\n%s',
      source_location[singular_idx],
      c_fmt_tag[singular_idx],
      wrap_msg('msgid', msgid[singular_idx], width, wrap_at_newline),
      wrap_msg('msgstr', msgstr[singular_idx], width, wrap_at_newline)
    )
    if (!all(singular_idx)) {
      msgid_plural = msgid_plural[!singular_idx]
      msgid1 = vapply(msgid_plural, `[`, character(1L), 1L)
      msgid2 = vapply(msgid_plural, `[`, character(1L), 2L)
      msgid_plural = vapply(
        msgstr_plural[!singular_idx],
        function(msgstr) paste( # nolint: brace_linter. TODO(lintr/2240): remove nolint.
          sprintf(msgstr_fmt, seq_along(msgstr) - 1L, msgstr),
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

wrap_msg = function(key, value, width=Inf, wrap_at_newline = TRUE) {
  out <- character(length(value))
  # xgettext always wraps at a newline (even if the whole message fits inside 'width')
  wrap_idx <- nchar(value) + nchar(key) + 3L > width
  if (wrap_at_newline) {
    wrap_idx <- wrap_idx | grepl("[\\]n.", value)
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
        sub_str[jj] = substr(str[ii], newline_indices[jj] + 1L, newline_indices[jj + 1L] - 2L)
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

make_src_location <- function(files, lines, message_source, use_base_rules) {
  if (use_base_rules && message_source == "R") return("")
  s <- paste(glue("{files}:{lines}"), collapse = " ")
  # branch above implies use_base_rules => message_source == "src"
  # 77 = 80 - nchar("#: "). 80 not 79 is for strwrap. NB: strwrap("012 345", width=4)
  paste0("#: ", if (use_base_rules) strwrap(s, width=77L) else s, "\n", collapse="")
}

# See https://www.gnu.org/software/gettext/manual/html_node/Header-Entry.html
#' @rdname write_po_file
#' @export
po_metadata = function(package='', version='', language='', author='', email='', bugs='', copyright = NULL, ...) {
  stopifnot(
    "copyright should be empty, a single name, or a list of components" =
      is.null(copyright) || is.character(copyright) || is.list(copyright)
  )
  pm = c(as.list(environment()), list(...))
  if (is.null(pm$charset)) pm$charset <- "UTF-8"
  if (is.null(pm$pot_timestamp)) pm$pot_timestamp <- Sys.time()
  if (is.null(pm$po_timestamp)) pm$po_timestamp <- pm$pot_timestamp
  if (is.null(pm$language_team)) pm$language_team <- pm$language
  class(pm) = 'po_metadata'
  pm
}

#' @rdname write_po_file
#' @export
format.po_metadata = function(x, template = FALSE, use_plurals = FALSE, ...) {
  if (template) {
    x$po_timestamp = "YEAR-MO-DA HO:MI+ZONE"
    x$author = "FULL NAME"
    x$email = "EMAIL@ADDRESS"
    x$language = ''
    x$language_team = "LANGUAGE <LL@li.org>"
  }
  if (is.character(x$copyright)) {
    x$copyright = list(years = format(x$pot_timestamp, "%Y"), holder = x$copyright)
  }
  copyright = build_copyright(x$copyright, template)
  keys = with(x, c(
    `Project-Id-Version` = glue("{package} {version}"),
    `Report-Msgid-Bugs-To` = bugs,
    `POT-Creation-Date` = maybe_make_time(pot_timestamp),
    `PO-Revision-Date` = maybe_make_time(po_timestamp),
    `Last-Translator` = if (nzchar(author) && nzchar(email)) glue("{author} <{email}>") else '',
    `Language-Team` = language_team,
    `Language` = language,
    `MIME-Version` = "1.0",
    `Content-Type` = glue("text/plain; charset={charset}"),
    `Content-Transfer-Encoding` = "8bit"
  ))
  if (use_plurals) {
    if (template) {
      keys["Plural-Forms"] =
        "nplurals=INTEGER; plural=EXPRESSION;"
    } else {
      keys["Plural-Forms"] = glue_data(
        get_lang_metadata(x$language),
        "nplurals={nplurals}; plural={plural};"
      )
    }
  }

  extra_keys = setdiff(
    names(x),
    c(
      "copyright", "package", "version", "bugs",
      "pot_timestamp", "po_timestamp",
      "author", "email", "language", "language_team", "charset"
    )
  )
  if (length(extra_keys)) keys = c(keys, setNames(unlist(x[extra_keys]), extra_keys))

  paste(
    c(
      copyright,
      wrap_msg("msgid", ""),
      wrap_msg("msgstr", paste(glue("{names(keys)}: {keys}\\n"), collapse = ""))
    ),
    collapse = "\n"
  )
}

#' @rdname write_po_file
#' @export
print.po_metadata = function(x, ...) writeLines(format(x, ...))

# apply format(), if the input is a timestamp. to flexibly allow po_timestamp to be a string or a POSIXct
maybe_make_time = function(x) if (inherits(x, 'POSIXt')) format(x, '%F %H:%M%z') else x

# see circa lines 2036-2046 of gettext/gettext-tools/src/xgettext.c for the copyright construction
build_copyright = function(copyright, template) {
  if (is.null(copyright)) return(character())
  if (template) {
    copyright = list(
      title = "SOME DESCRIPTIVE TITLE.",
      years = "YEAR",
      holder = if (is.list(copyright)) copyright$holder else copyright,
      additional = 'FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.'
    )
  }
  copyright <- paste(
    "#",
    c(
      copyright$title,
      glue_data(copyright, "Copyright (C) {years} {holder}"),
      "This file is distributed under the same license as the R package.",
      copyright$additional
    )
  )
  # see https://stackoverflow.com/q/15653093/3576984
  # not added above because #, is incongruent
  if (template) copyright <- c(copyright, "#", "#, fuzzy")
  copyright
}
