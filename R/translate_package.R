translate_package = function(
  dir='.', languages,
  copyright, bugs, verbose=FALSE
) {
  check_sys_reqs()

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = is.character(dir),
    "'languages' must be a character vector" = missing(languages) || is.character(languages)
  )

  dir = get_directory(dir)

  desc_data = get_desc_data(dir)
  package <- desc_data['Package']
  version <- desc_data['Version']

  potfile <- character()
  update = file.exists(podir <- file.path(dir, 'po')) &&
    length(potfile <- list.files(podir, pattern='^R.*\\.pot$'))

  if (verbose) {
    if (update) {
      message(domain=NA, gettextf(
        "Updating translation template for package '%s' (last updated %s)",
        package,
        format(file.info(potfile)$atime),
        domain='R-potools'
      ))
    } else {
      message(domain=NA, gettextf("Starting translations for package '%s'", package, domain='R-potools'))
    }
  }
  if (!update) dir.create(podir, showWarnings = FALSE)

  if (verbose) message('Getting R-level messages...')
  message_data = get_r_messages(dir, verbose = verbose)

  if (!nrow(message_data)) {
    if (verbose) message('No messages to translate; finishing')
    return(invisible())
  }

  # for testing, we need a connection that stays open so that readLines(n=1L)
  #   reads successive lines. originally tried passing a test connection as
  #   an argument to prompt(), but that closed the connection each time -->
  #   only the first line is ever read.
  set_prompt_conn()
  on.exit(unset_prompt_conn())

  if (verbose) message('Running message diagnostics...')

  # check for calls like stop("a", i, "b", j) that are better suited for
  #   translation as calls like
  exit =
    message_data[type == 'singular', if (.N > 1L) .(msgid), by=.(file, call)
                 ][ , {
                   if (.N > 0L && verbose) message(domain=NA, gettextf(
                     'Found %d messaging calls that might be better suited for gettextf for ease of translation:',
                     uniqueN(call), domain='R-potools'
                   ))
                   .SD
                 }][ , by = .(file, call), {
                   cat(gettextf(
                     '\nMulti-string call:\n%s\n< File:%s >\nPotential replacement with gettextf():\n%s\n',
                     call_color(.BY$call),
                     file_color(.BY$file),
                     build_gettextf_color(build_gettextf_call(.BY$call, package))
                   ))
                   TRUE
                 }][ , if (.N > 0L) prompt('Exit now to repair any of these? [y/N]') else 'n']
  if (tolower(exit) %chin% c('y', 'yes')) return(invisible())

  if (verbose) message('Running tools::update_pkg_po()')
  tools::update_pkg_po(dir, package, version, copyright, bugs)

  if (missing(languages)) {
    if (verbose) message('No languages provided; finishing')
    return(invisible())
  }

  for (language in languages) {
    metadata = KNOWN_LANGUAGES[.(language)]
    # overwrite any existing translations written in previous translation.
    #   set blank initially (rather than deleting the column) to allow
    #   for interrupting the translation -- if unset, write_po_file will
    #   fail if both these columns are not yet present.
    message_data[type == 'singular', msgstr := ""]
    message_data[type == 'plural', plural_msgstr := .(list(rep("", metadata$nplurals)))]
    lang_file <- file.path(dir, 'po', sprintf("R-%s.po", language))
    if (update && file.exists(lang_file)) {
      if (verbose) {
        message(domain=NA, gettextf(
          'Found existing translations for %s (%s/%s) in %s',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file, domain='R-potools'
        ))
      }
      old_message_data = get_po_messages(lang_file)

      if (any(idx <- old_message_data$fuzzy == 2L)) {
        message(domain=NA, gettextf(
          'Found %d translations marked as deprecated in %s.',
          sum(idx), lang_file, domain='R-potools'
        ))
        message('Typically, this means the corresponding error messages have been refactored.')
        message('Reproducing these messages here for your reference since they might still provide some utility.')

        dashes = strrep('-', .9*getOption('width'))
        old_message_data[idx & type == 'singular', {
          if (.N > 0L) {
            message(' ** SINGULAR MESSAGES **')
            cat(rbind(dashes, msgid, msgstr), sep='\n')
          }
        }]
        old_message_data[idx & type == 'plural', {
          if (.N > 0L) {
            message(' ** PLURAL MESSAGES **')
            cat(do.call(rbind, c(list(dashes), plural_msgid, plural_msgstr)), sep='\n')
          }
        }]

        old_message_data = old_message_data[(!idx)]
      }

      message_data[
        old_message_data[type == 'singular'],
        on = c('type', 'msgid'),
        `:=`(msgstr = i.msgstr, fuzzy = i.fuzzy)
      ]
      # can't join on lists :\
      if (!all(vapply(old_message_data$plural_msgstr, is.null, logical(1L)))) {
        message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
        old_message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
        message_data[
          old_message_data[type == 'plural'],
          on = c('type', 'join_id'),
          `:=`(plural_msgstr = i.plural_msgstr, fuzzy = i.fuzzy)
        ]

        message_data[ , 'join_id' := NULL]
        old_message_data[ , 'join_id' := NULL]
      }
      new_idx = message_data[
        fuzzy == 1L |
          (type == 'singular' & !nzchar(msgstr)) |
          (type == 'plural' & !vapply(plural_msgstr, function(x) all(nzchar(x)), logical(1L))),
        which = TRUE
      ]
    } else {
      new_idx = seq_len(nrow(message_data))
      message_data[ , fuzzy := 0L]
    }
    if (!length(new_idx)) {
      if (verbose) message(domain=NA, gettextf(
        'Translations for %s are up to date! Skipping.', language, domain='R-potools'
      ))
      next
    }
    if (verbose) {
      message(domain=NA, gettextf(
        'Beginning new translations for %s (%s/%s); found %d untranslated messages',
        language, metadata$full_name_eng, metadata$full_name_native, length(new_idx), domain='R-potools'
      ))
      message("(To quit translating, press 'Esc'; progress will be saved)")
    }

    author = prompt('Thanks! Who should be credited with these translations?')
    email = prompt('And what is their email?')
    author = sprintf("%s <%s>", author, email)

    # on.exit this to allow ESC to quit mid-translation. the intent is for the
    #   on.exit command to be overwritten on each iteration over languages --
    #   only one partially-finished language should be written at a time.
    INCOMPLETE = TRUE
    on.exit({
      if (INCOMPLETE) write_po_file(message_data, lang_file, package, version, author, metadata) # nocov
      # since add=FALSE, we overwrite the above call; duplicate it here
      unset_prompt_conn()
    })

    if (verbose) {
      message(
        "***************************\n",
        "** BEGINNING TRANSLATION **\n",
        "***************************\n\n",
        "Some helpful reminders:\n",
        " * You can skip a translation by entering nothing (just press RETURN)",
        " * Special characters (like newlines, \\n, or tabs, \\t) should be written just like that (with an escape)",
        " * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages",
        " * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here'",
        " * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below"
      )
    }
    # NB: loop over rows to facilitate quitting without losing progress
    for (ii in new_idx) {
      if (message_data$type[ii] == 'plural') {
        translation = read_translation(
          message_data$plural_msgid[[ii]][1L],
          'plural',
          message_data$file[ii],
          message_data$call[ii],
          message_data$fuzzy[ii],
          message_data$plural_msgstr[[ii]],
          metadata
        )
        set(message_data, ii, 'plural_msgstr', list(translation))
      } else if (!message_data$is_repeat[ii]) {
        translation = read_translation(
          message_data$msgid[ii],
          'singular',
          message_data$file[ii],
          message_data$call[ii],
          message_data$fuzzy[ii],
          message_data$msgstr[ii],
          metadata
        )
        set(message_data, ii, 'msgstr', translation)
      }
    }

    # set INCOMPLETE after write_po_file for the event of a process interruption
    #   between the loop finishing and the write_po_file command executing
    write_po_file(message_data, lang_file, package, version, author, metadata)
    INCOMPLETE = FALSE
  }

  if (verbose) message('Re-running tools::update_pkg_po() to update .mo files')
  tools::update_pkg_po(dir, package, version, copyright, bugs)
}

# just here to generate translations. comes from the PLURAL_RANGE_STRINGS csv
invisible({
  gettext("independently of n", domain="R-potools")
  gettext("when n = 1", domain="R-potools")
  gettext("when n is not 1", domain="R-potools")
  gettext("when n is 0 or 1", domain="R-potools")
  gettext("when n is at bigger than 1", domain="R-potools")
  gettext("when n = 2-4, 22-24, 32-34, ...", domain="R-potools")
  gettext("when n = 0, 5-21, 25-31, 35-41, ...", domain="R-potools")
  gettext("when n = 1, 21, 31, 41, ...", domain="R-potools")
  gettext("when n = 1, 21, 31, 41, ...", domain="R-potools")
  gettext("when n = 0, 5-20, 25-30, 35-40, ...", domain="R-potools")
})

# take from those present in r-devel:
# ls -1 ~/svn/R-devel/src/library/*/po/*.po | \
#   awk -F"[./]" '{print $10}' | \
#   sed -r 's/^R(Gui)?-//g' | sort -u | \
#   awk '{print "  ", $1, " = ,"}'
# alternatively, a more complete list can be found on some websites:
#   https://saimana.com/list-of-country-locale-code/
# nplurals,plural info from https://l10n.gnome.org/teams/<language>
# NB: looks may be deceiving for right-to-left scripts (e.g. Farsi), where the
#   displayed below might not be in the order it is parsed.
KNOWN_LANGUAGES = fread(system.file('extdata', 'language_metadata.csv', package='potools'), key='code')

# the 'plural' column above is designed for computers;
#   translate that to something human-legible here.
# NB: 'plural' is 0-based (like in the .po file), but
#   'plural_index' is 1-based (to match the above R-level code).
PLURAL_RANGE_STRINGS = fread(
  system.file('extdata', 'plurals_metadata.csv', package='potools'),
  key = c('plural', 'plural_index')
)
