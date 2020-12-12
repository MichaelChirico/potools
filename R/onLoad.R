globalVariables(
  c('msgid', 'msgstr', 'i.msgstr', 'plural_msgid', 'plural_msgstr', 'i.plural_msgstr', 'type', '.'),
  package = 'potools'
)

# to be overwritten by .onLoad
red = white = blue = green = yellow = identity = function(...) NULL
KNOWN_LANGUAGES = PLURAL_RANGE_STRINGS = NULL
.onLoad <- function(libname, pkgname) {
  if (requireNamespace('crayon', quietly = TRUE)) {
    red = crayon::red
    white = crayon::white
    blue = crayon::blue
    green = crayon::green
    yellow = crayon::yellow
  } else {
    red = white = blue = green = yellow = identity
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
  KNOWN_LANGUAGES = fread(system.file('extdata', 'language_metadata.csv', package='potools'), key='code')

  # the 'plural' column above is designed for computers;
  #   translate that to something human-legible here.
  # NB: 'plural' is 0-based (like in the .po file), but
  #   'plural_index' is 1-based (to match the above R-level code).
  PLURAL_RANGE_STRINGS = fread(
    system.file('extdata', 'plurals_metadata.csv', package='potools'),
    key = c('plural', 'plural_index')
  )
}
