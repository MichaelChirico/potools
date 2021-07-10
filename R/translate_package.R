translate_package = function(
  dir = '.', languages,
  diagnostics = list(check_cracked_messages, check_untranslated_cat, check_untranslated_src),
  custom_translation_functions = list(R = NULL, src = NULL),
  max_translations = Inf,
  use_base_rules = package %chin% .potools$base_package_names,
  copyright = NULL, bugs = '', verbose = FALSE
) {
  result <- check_potools_sys_reqs()
  if (!isTRUE(result)) stop(result) # nocov

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = is.character(dir),
    "'languages' must be a character vector" = missing(languages) || is.character(languages),
    "'diagnostics' should be empty, a function, or list of functions" =
      is.null(diagnostics)
      || is.function(diagnostics)
      || (is.list(diagnostics) && all(vapply(diagnostics, is.function, logical(1L))))
  )

  # en-list singleton diagnostic for convenience
  if (is.function(diagnostics)) diagnostics = list(diagnostics)

  dir = get_directory(dir)

  desc_data = get_desc_data(dir)
  package <- desc_data['Package']
  is_base <- package == 'base'
  version <- desc_data['Version']

  po_dir <- file.path(dir, 'po')
  r_potfile <- file.path(po_dir, sprintf("R-%s.pot", package))
  src_potfile <- file.path(po_dir, sprintf("%s.pot", if (is_base) 'R' else package))
  update = file.exists(po_dir) && (file.exists(r_potfile) || file.exists(src_potfile))

  if (verbose) {
    if (update) {
      # is it worthwhile to try and distinguish the creation time of the
      #  R pot file and the src pot file? probably not...
      messagef(
        "Updating translation template for package '%s' (last updated %s)",
        package,
        format(file.info(r_potfile)$atime)
      )
    } else {
      messagef("Starting translations for package '%s'", package)
    }
  }
  if (!update) dir.create(po_dir, showWarnings = FALSE)

  message_data = get_message_data(dir, custom_translation_functions, verbose=verbose)

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

  for (diagnostic in diagnostics) {
    diagnostic <- match.fun(diagnostic)
    result <- diagnostic(message_data)
    if (!nrow(result)) next
    show_diagnostic_results(result, diagnostic)
    if (tolower(prompt('Exit now to repair any of these? [y/N]')) %chin% c('y', 'yes')) return(invisible())
  }

  if (verbose) message('Generating .pot files...')
  po_params = list(package = package, version = version, copyright = copyright, bugs = bugs)
  write_po_files(message_data, po_dir, po_params, template = TRUE, use_base_rules = use_base_rules)

  if (l10n_info()[["UTF-8"]]) {
    # on UTF-8 machines we install the en@quot messages too
    # TODO: streamline this -- en_quote is definitely doing some redundant stuff
    message('Generating en@quot translations')
    update_en_quot_mo_files(dir, verbose)
  }


  if (missing(languages)) {
    if (verbose) message('No languages provided; finishing')
    return(invisible())
  }

  for (language in languages) {
    metadata = get_lang_metadata(language)
    po_params$language = language

    # overwrite any existing translations written in previous translation.
    #   set blank initially (rather than deleting the column) to allow
    #   for interrupting the translation -- if unset, write_po_files will
    #   fail if both these columns are not yet present.
    message_data[type == 'singular', 'msgstr' := ""]
    message_data[type == 'plural', 'msgstr_plural' := .(list(rep("", metadata$nplurals)))]

    lang_file <- file.path(po_dir, sprintf("R-%s.po", language))
    if (update && file.exists(lang_file)) {
      if (verbose) {
        messagef(
          'Found existing R translations for %s (%s/%s) in %s. Running msgmerge...',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file
        )
      }
      run_msgmerge(lang_file, r_potfile)

      find_fuzzy_messages(message_data, lang_file)
    } else {
      message_data[message_source == "R", 'fuzzy' := 0L]
    }

    lang_file <- file.path(po_dir, sprintf("%s.po", language))
    if (update && file.exists(lang_file)) {
      if (verbose) {
        messagef(
          'Found existing src translations for %s (%s/%s) in %s. Running msgmerge...',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file
        )
      }
      run_msgmerge(lang_file, src_potfile)

      find_fuzzy_messages(message_data, lang_file)
    } else {
      message_data[message_source == "src", 'fuzzy' := 0L]
    }

    new_idx = message_data[
      is_marked_for_translation & (
        fuzzy == 1L
        | (type == 'singular' & !nzchar(msgstr) & nzchar(msgid, keepNA = TRUE))
        | (type == 'plural' & !vapply(msgstr_plural, function(x) all(nzchar(x)), logical(1L)))
      ),
      which = TRUE
    ]

    if (!length(new_idx)) {
      if (verbose) messagef('Translations for %s are up to date! Skipping.', language)
      next
    }
    if (verbose) {
      messagef(
        'Beginning new translations for %s (%s/%s); found %d untranslated messages',
        language, metadata$full_name_eng, metadata$full_name_native, length(new_idx)
      )
      message("(To quit translating, press 'Esc'; progress will be saved)")
    }

    po_params$author = prompt('Thanks! Who should be credited with these translations?')
    po_params$email = prompt('And what is their email?')

    # on.exit this to allow ESC to quit mid-translation. the intent is for the
    #   on.exit command to be overwritten on each iteration over languages --
    #   only one partially-finished language should be written at a time.
    INCOMPLETE = TRUE
    on.exit({
      if (INCOMPLETE) write_po_files(message_data, po_dir, po_params, use_base_rules = use_base_rules) # nocov
      # since add=FALSE, we overwrite the above call; duplicate it here
      unset_prompt_conn()
    })

    if (verbose) {
      message(
        "***************************\n",
        "** BEGINNING TRANSLATION **\n",
        "***************************\n\n",
        "Some helpful reminders:\n",
        " * You can skip a translation by entering nothing (just press RETURN)\n",
        " * Special characters (like newlines, \\n, or tabs, \\t) should be written just like that (with an escape)\n",
        " * Be sure to match message templates. The count of templates (%s, %d, etc.) must match in all languages, as must initial and terminal newlines (\\n)\n",
        " * While the count of templates must match, the _order_ can be changed by using e.g. %2$s to mean 'use the second input as a string here'\n",
        " * Whenever templates or escaping is happening in a string, these will be 'highlighted' by carets (^) in the line below"
      )
    }
    # NB: loop over rows to facilitate quitting without losing progress
    for (ii in head(new_idx, max_translations)) {
      if (message_data$type[ii] == 'plural') {
        translation = read_translation(
          message_data$msgid_plural[[ii]][1L],
          'plural',
          message_data$file[ii],
          message_data$call[ii],
          message_data$fuzzy[ii],
          message_data$msgstr_plural[[ii]],
          metadata
        )
        set(message_data, ii, 'msgstr_plural', list(translation))
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

    # set INCOMPLETE after write_po_files for the event of a process interruption
    #   between the loop finishing and the write_po_files command executing
    write_po_files(message_data, po_dir, po_params, use_base_rules = use_base_rules)
    INCOMPLETE = FALSE
  }

  if (verbose) message('"Installing" translations with msgfmt')
  # TODO: reinstate source marker tags, at least for src .pot file & maybe for R .pot file too?
  update_mo_files(dir, package, verbose = verbose)
  return(invisible())
}

# just here to generate translations. comes from the PLURAL_RANGE_STRINGS csv
# cat(sprintf(
#   "invisible({\n%s\n})\n",
#   paste(
#     sprintf(
#       '  gettext("%s")',
#       unique(sort(data.table::fread("inst/extdata/plurals_metadata.csv")$range))
#     ),
#     collapse = "\n"
#   )
# ))
invisible({
  gettext("independently of n")
  gettext("when n = 0")
  gettext("when n = 0, 5-20, 25-30, 35-40, ...")
  gettext("when n = 0, 5-21, 25-31, 35-41, ...")
  gettext("when n = 1")
  gettext("when n = 1, 21, 31, 41, ...")
  gettext("when n = 100-102, 200-202, 300-302, ...")
  gettext("when n = 11-99, 111-199, 211-299, ...")
  gettext("when n = 2")
  gettext("when n = 2-4, 22-24, 32-34, ...")
  gettext("when n = 3-10, 103-110, 203-210, ...")
  gettext("when n is 0 or 1")
  gettext("when n is at bigger than 1")
  gettext("when n is not 1")
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
# assign to .potools, not a package env, to keep more readily mutable inside update_metadata()
.potools$KNOWN_LANGUAGES = fread(system.file('extdata', 'language_metadata.csv', package='potools'), key='code')

# the 'plural' column above is designed for computers;
#   translate that to something human-legible here.
# NB: 'plural' is 0-based (like in the .po file), but
#   'plural_index' is 1-based (to match the above R-level code).
# assign to .potools, not a package env, to keep more readily mutable inside update_metadata()
.potools$PLURAL_RANGE_STRINGS = fread(
  system.file('extdata', 'plurals_metadata.csv', package='potools'),
  key = c('plural', 'plural_index')
)

# for testing; unexported
# nocov start
reset_language_metadata = function() {
  .potools$KNOWN_LANGUAGES = fread(
    system.file('extdata', 'language_metadata.csv', package='potools'),
    key='code'
  )
  .potools$PLURAL_RANGE_STRINGS = fread(
    system.file('extdata', 'plurals_metadata.csv', package='potools'),
    key = c('plural', 'plural_index')
  )
}
# nocov end
