translate_package = function(
  dir='.', languages,
  copyright, bugs, verbose=FALSE
) {
  if (!interactive()) {
    stop('This is an interactive function. For non-interactive use cases, start from tools::update_pkg_po.')
  }
  check_sys_reqs()

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = is.character(dir),
    "'languages' must be a characer vector" = missing(languages) || is.character(languages)
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

  # check for calls like stop("a", i, "b", j) that are better suited for
  #   translation as calls like
  exit =
    message_data[type == 'singular', if (.N > 1L) .(msgid), by=.(file, call)
                 ][ , {
                   if (.N > 0L && verbose) message(domain=NA, gettextf(
                     'Found %d messaging calls that might be better suited to use gettextf for ease of translation:',
                     uniqueN(call), domain='R-potools'
                   ))
                   .SD
                 }][ , by = .(file, call), {
                   cat(gettextf(
                     '\nMulti-string call:\n%s\n< File:%s >\nPotential replacement with gettextf():\n%s\n',
                     red(.BY$call), white(.BY$file), blue(build_gettextf_call(.BY$call, package))
                   ))
                   TRUE
                 }][ , if (.N > 0L) readline('Exit now to repair any of these? [Y/n]') else 'n']
  if (!nzchar(exit) || tolower(exit) %chin% c('y', 'yes')) return(invisible())

  if (!nrow(message_data)) {
    if (verbose) message('No messages to translate; finishing')
    return(invisible())
  }

  if (verbose) message('Running message diagnostics...')


  if (verbose) message('Running tools::update_pkg_po()')
  tools::update_pkg_po(dir, package, version, copyright, bugs)

  if (missing(languages)) {
    if (verbose) message('No languages provided; finishing')
    return(invisible())
  }

  for (language in languages) {
    metadata = KNOWN_LANGUAGES[.(language)]
    if ('msgstr' %chin% names(message_data)) message_data[ , 'msgstr' := NULL]
    if ('plural_msgstr' %chin% names(message_data)) message_data[ , 'plural_msgstr' := NULL]

    if (update && file.exists(lang_file <- file.path(dir, 'po', sprintf("R-%s.po", language)))) {
      if (verbose) {
        message(domain=NA, gettextf(
          'Found existing translations for %s (%s/%s) in %s',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file, domain='R-potools'
        ))
      }
      old_message_data = get_po_messages(lang_file)
      message_data[old_message_data[type == 'singular'], on = c('type', 'msgid'), msgstr := i.msgstr]
      # can't join on lists :\
      if (!all(vapply(old_message_data$plural_msgstr, is.null, logical(1L)))) {
        message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
        old_message_data[ , 'join_id' := vapply(plural_msgid, paste, character(1L), collapse='|||')]
        message_data[old_message_data[type == 'plural'], on = c('type', 'join_id'), plural_msgstr := i.plural_msgstr]

        message_data[ , 'join_id' := NULL]
        old_message_data[ , 'join_id' := NULL]
      }
      new_idx = message_data[
        (type == 'singular' & is.na(msgstr)) |
          (type == 'plural' & !lengths(plural_msgstr)),
        which = TRUE
      ]
    } else {
      new_idx = seq_len(nrow(message_data))
    }
    if (!length(new_idx)) {
      if (verbose) message(domain=NA, gettextf(
        'Translations for %s up to date! Skipping.', language, domain='R-potools'
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
    # go row-wise to facilitate quitting without losing progress
    # TODO: check message templates for consistency
    # TODO: default value is to set msgstr=msgid
    # TODO: need more spacing between message prompts
    for (ii in new_idx) {
      if (message_data$type[ii] == 'plural') {
        translation <- vector('list', metadata$nplurals)
        for (jj in seq_len(metadata$nplurals)) {
          translation[[jj]] = readline(gettextf(
            'File: %s\nCall: %s\nPlural message: "%s"\nHow would you translate this message into %s %s?',
            white(message_data$file[ii]),
            green(message_data$call[ii]),
            red(message_data$plural_msgid[ii][[1L]]),
            blue(metadata$full_name_eng),
            yellow(PLURAL_RANGE_STRINGS[.(metadata$plural, jj), range]),
            domain = "R-potools"
          ))
        }
        set(message_data, ii, 'plural_msgstr', list(translation))
      } else if (!message_data$is_repeat[ii]) {
        translation = readline(gettextf(
          'File: %s\nCall: %s\nMessage: "%s"\nHow would you translate this message into %s?',
          white(message_data$file[ii]),
          green(message_data$call[ii]),
          red(message_data$msgid[ii]),
          blue(metadata$full_name_eng),
          domain = "R-potools"
        ))
        set(message_data, ii, 'msgstr', translation)
      }
    }

    author = readline('Thanks! Who should be credited with these translations?')
    email = readline('And what is their email?')
    author = sprintf("%s <%s>", author, email)
    write_po_file(message_data, lang_file, package, version, author, metadata)
  }

  if (verbose) message('Re-running tools::update_pkg_po() to update .mo files')
  tools::update_pkg_po(dir, package, version, copyright, bugs)
}

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
KNOWN_LANGUAGES = fread("
code,full_name_eng,full_name_native,nplurals,plural
da,Danish,Dansk,2,(n!=1)
de,German,Deutsch,2,(n!=1)
en,English,English,2,(n!=1)
en_GB,British English,British English,2,(n!=1)
es,Spanish,Español,2,(n!=1)
fa,Farsi,فارسی,1,0
fr,French,Français,2,(n>1)
it,Italian,Italiano,2,(n!=1)
ja,Japanese,日本語,1,0
ko,Korean,한국어,1,0
nn,Dutch,Nederlands,2,(n!=1)
pl,Polish,Polski,3,(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)
pt_BR,Brazilian Portugese,Português Brasileiro,2,(n>1)
ru,Russian,Русский,3,(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20)? 1 : 2)
tr,Turkish,Türkçe,1,0
zh_CN,Mainland Chinese,普通话,1,0
zh_TW,Taiwanese Chinese,台湾话,1,0
", key='code')

# the 'plural' column above is designed for computers;
#   translate that to something human-legible here.
# NB: 'plural' is 0-based (like in the .po file), but
#   'plural_index' is 1-based (to match the above R-level code).
# NB: sep="#"... maybe a bad choice... but keeps the lines to 120
#   characters and doesn't cause issues from sep=',' with , in text
PLURAL_RANGE_STRINGS = fread("
plural#plural_index#range
0#0#independently of n
(n!=1)#0#when n = 1
(n!=1)#1#when n is not 1
(n>1)#0#when n is 0 or 1
(n>1)#1#when n is at bigger than 1
(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#0#when n = 1
(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#1#when n = 2-4, 22-24, 32-34, ...
(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#2#when n = 0, 5-21, 25-31, 35-41, ...
(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#0#when n = 1, 21, 31, 41, ...
(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#1#when n = 2-4, 22-24, 32-34, ...
(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)#2#when n = 0, 5-20, 25-30, 35-40, ...
", sep='#', key=c('plural', 'plural_index'))

# just here to generate translations
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
