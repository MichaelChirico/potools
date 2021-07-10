# extend metadata so that an unrecognized language can be used for translation. also
#   stores this metadata in the package environment so it can be reused for the session duration.
update_metadata = function(language) {
  metadata = data.table(code = language)

  messagef("'%s' is not a known language. ", language, appendLF=FALSE)
  # perhaps refer to http://docs.translatehouse.org/projects/localization-guide/en/latest/l10n/pluralforms.html instead
  message("Please help supply some metadata about it. You can check https://l10n.gnome.org/teams/<language>")
  metadata[ , "full_name_eng" := prompt("How would you refer to this language in English?")]
  metadata[ , "full_name_native" := prompt("How would you refer to this language in the language itself?")]
  metadata[ , "nplurals" := prompt(
    "How many pluralizations are there for this language [nplurals]?",
    require_type = "integer"
  )]

  metadata[ , "plural" := prompt("What is the rule for deciding which plural applies as a function of n [plural]?")]
  known_plurals = unique(.potools$PLURAL_RANGE_STRINGS$plural)
  if (metadata$plural %chin% known_plurals) {
  } else if ((idx <- chmatch(strip_ws(metadata$plural), strip_ws(.potools$PLURAL_RANGE_STRINGS$plural), 0L)) > 0L) {
    # inexact match found; update so that the right row of PLURAL_RANGE_STRINGS is used during translation
    # NB: chmatch() returns the _first_ match, i.e., we definitely have length-1 output
    metadata[ , "plural" := .potools$PLURAL_RANGE_STRINGS$plural[idx]]
  } else {
    messagef(
      "Supplied 'plural':\n%s\nDid not match any known 'plural's:\n%s\nUsing generic description of cases instead.",
      metadata$plural, paste(known_plurals, collapse = '\n')
    )
    plural_index = 0:(metadata$nplurals - 1L)
    .potools$PLURAL_RANGE_STRINGS <- rbind(
      .potools$PLURAL_RANGE_STRINGS,
      data.table(
        plural = metadata$plural,
        plural_index = plural_index,
        range = paste0("for n where 'plural' resolves to ", plural_index)
      )
    )
    setkeyv(.potools$PLURAL_RANGE_STRINGS, c("plural", "plural_index"))
  }
  .potools$KNOWN_LANGUAGES = rbind(.potools$KNOWN_LANGUAGES, metadata)
  setkeyv(.potools$KNOWN_LANGUAGES, "code")
  message("Thanks! Please file an issue on GitHub to get this language recognized permanently")
  metadata
}

strip_ws = function(s) gsub(' ', '', s, fixed = TRUE)
