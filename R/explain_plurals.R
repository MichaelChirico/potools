#' Explain plural message criteria verbally
#'
#' The `nplural` syntax in .po file metadata can be hard to grok,
#'   even for native speakers. This function tries to de-mystify
#'   this by providing verbal expressions of which numbers apply
#'   to which index in the `msgstr` array.
#'
#' @param language A single locale code. See [translate_package()] for details.
#' @param index Optional. If supplied, a 0-based index to
#'   explain for a given language. If not supplied, all plurals
#'   for the supplied language are described.
#' @export
po_explain_plurals <- function(language, index) {
  stopifnot(
    "Supply one language code (see ?translate_package)" =
      is.character(language) && length(language) == 1L,
    "If supplied, `index` should be a single non-negative number" =
      missing(index) || (is.numeric(index) && length(index) == 1L && index > 0L)
  )
  language_metadata <- .potools$KNOWN_LANGUAGES[.(language), nomatch = NULL]
  if (!nrow(language_metadata)) {
    stopf("%s is not a known language. Please file a GitHub issue to change this.", language)
  }

  plural_metadata = .potools$PLURAL_RANGE_STRINGS[language_metadata, .SD, on = "plural"]
  if (missing(index)) {
    language_metadata[, message(domain = NA, sprintf(
      ngettext(nplurals, "%s (%s) has %d plural form.", "%s (%s) has %d plural forms."),
      full_name_eng, full_name_native, nplurals
    ))]
    plural_metadata[, message(domain = NA, paste(
      gettextf("  - plural_index = %d applies %s", plural_index, range_translation = gettext(range)),
      collapse = "\n"
    ))]
  } else {
    if (index >= language_metadata$nplurals) stopf(
      "Requested 0-based plural index %d, but %s only has %d plural forms",
      index, language, language_metadata$nplurals
    )
    plural_metadata[
      .(plural_index = as.integer(index)),
      on = 'plural_index',
      messagef(
        "For %s (%s), plural index %d applies %s",
        language_metadata$full_name_eng, language_metadata$full_name_native,
        plural_index, range_translation = gettext(range)
      )
    ]
  }
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
