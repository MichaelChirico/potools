translate_package = function(
  dir='.', languages, patch_calls=TRUE,
  copyright, bugs, verbose=FALSE,
) {
  if (!interactive()) {
    stop('This is an interactive function. For non-interactive use cases, start from tools::update_pkg_po.')
  }
  if (any(is_missing <- !nzchar(Sys.which(SYSTEM_REQUIREMENTS)))) {
    if (.Platform$OS.type == 'windows') {
      platform_msg = gettextf(
        'These tools are available as an Rtools goodie, check %s',
        RTOOLS_URL, domain='R-potools'
      )
    } else {
      if (Sys.info()['sysname'] == 'Darwin') {
        platform_msg = gettext(
          'These GNU tools are commonly available, try installing from brew or apt-get', domain='R-potools'
        )
      } else {
        platform_msg = gettext(
          'These GNU tools are commonly available from the Linux package manager for your system', domain='R-potools'
        )
      }
    }
    stop(domain = NA, gettextf(
      'Missing (or not on PATH) system requirements %s.\n%s',
      toString(SYSTEM_REQUIREMENTS[is_missing]), platform_msg, domain='R-potools'
    ))
  }

  stopifnot(
    'Only one package at a time' = length(dir) == 1L,
    "'dir' must be a character" = is.character(dir),
    "'languages' must be a characer vector" = missing(languages) || !is.character(languages)
  )
  dir = normalizePath(dir)

  if (!file.exists(dir)) {
    stop(domain=NA, gettextf('%s does not exist', dir, domain='R-potools'))
  }
  if (!file.info(dir)$isdir) {
    stop(domain=NA, gettextf('%s is not a directory', dir, domain='R-potools'))
  }
  if (!file.exists(desc_file <- file.path(dir, 'DESCRIPTION'))) {
    stop(domain=NA, gettextf('%s is not a package (missing DESCRIPTION', dir, domain='R-potools'))
  }
  desc_data <- read.dcf(desc_file, c('Package', 'Version'))[1L, ]
  if (anyNA(desc_data)) {
    stop(domain=NA, gettextf(
      '%s is not a package (missing Package or Version field in DESCRIPTION', dir, domain='R-potools'
    ))
  }
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

  if (verbose) message('Running tools::update_pkg_po()')
  tools::update_pkg_po(dir, package, version, copyright, bugs)

  if (missing(languages)) {
    if (verbose) message('No languages provided; finishing')
    return(invisible())
  }

  for (language in languages) {
    metadata = KNOWN_LANGUAGES[.(langua)]
    if (update && file.exists(lang_file <- file.path(dir, 'po', sprintf("R-%s.po", language)))) {
      if (verbose) {
        message(domain=NA, gettextf(
          'Found existing translations for %s (%s/%s) in %s',
          language, metadata$full_name_eng, metadata$full_name_native, lang_file, domain='R-potools'
        ))
      }
      old_message_data = get_po_messages(lang_file)
    } else {
      if (verbose) {
        message(domain=NA, gettextf(
          'Beginning new translations for %s (%s/%s)',
          language, metadata$full_name_eng, metadata$full_name_native, domain='R-potools'
        ))
      }
      quit_str = gettext("(To quit translating, press 'Esc'; progress is saved)", domain="R-potools")
      # go row-wise to facilitate quitting without losing progress
      for (ii in seq_len(nrow(message_data))) {
        if (message_data$type[ii] == 'plural') {
          translations <- character(metadata$nplurals)
          for (jj in seq_len(metadata$nplurals)) {
            translation[jj] <- readline(gettextf(
              "Translating plural message %s inside call %s in %s.\nHow would you translate this message into %s %s?"
            ))
          }
        }
      }
      message_data[ , by = names(message_data), {
        msgstr := if (.BY$type == 'plural') {
          readline(sprintf("hi"))
        } else {
          readline("bye")
        }
      }]
    }
  }
}

# see ?tools::update_pkg_po
SYSTEM_REQUIREMENTS = c('xgettext', 'msgmerge', 'msgfmt', 'msginit', 'msgconv')
RTOOLS_URL = 'https://www.stats.ox.ac.uk/pub/Rtools/goodies/gettext-tools.zip'

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
PLURAL_RANGE_STRINGS = fread("
plural,plural_index,range
0,0,independently of n
(n!=1),0,when n = 1
(n!=1),1,when n is not 1
(n>1),0,when n is 0 or 1
(n>1),1,when n is at bigger than 1
(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2),0,
")

PO_HEADER_TEMPLATE = 'msgid ""
msgstr ""
"Project-Id-Version: %s %s\n"
"POT-Creation-Date: %s\n"
"PO-Revision-Date: %s\n"
"Last-Translator: %s\n"
"Language-Team: %s\n"
"Language: %s\n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"
"Plural-Forms: nplurals=%d; plural=%s;\n"
'
