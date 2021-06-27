# extend metadata so that an unrecognized language can be used for translation. also
#   stores this metadata in the package environment so it can be reused for the session duration.
add_new_metadata = function(metadata, language) {
  messagef("'%s' is not a known language. ", language, appendLF=FALSE)
  message("Please help supply some metadata about it. You can check https://l10n.gnome.org/teams/<language>")
  metadata[ , "full_name_eng" := prompt("How would you refer to this language in English?")]
  metadata[ , "full_name_native" := prompt("How would you refer to this language in the language itself?")]
  metadata[ , "nplurals" := prompt(
    "How many pluralizations are there for this language [nplurals]?",
    require_type = "integer"
  )]
  metadata[ , "plural" := prompt("What is the rule for deciding which plural applies as a function of n [plural]?")]
  if (!metadata$plural %chin% PLURAL_RANGE_STRINGS$plural) {
    messagef(
      "Supplied 'plural':\n%s\nDid not match any known 'plural's:\n%s\nUsing generic description of cases instead.",
      metadata$plural, paste(unique(PLURAL_RANGE_STRINGS$plural), collapse = '\n')
    )
    plural_index = 0:(metadata$nplurals - 1L)
    # not used in this function, so just x <- rbind(...) will only
    #   overwrite locally, so we have to unlock the binding
    unlockBinding("PLURAL_RANGE_STRINGS", asNamespace("potools"))
    PLURAL_RANGE_STRINGS <<- rbind(
      PLURAL_RANGE_STRINGS,
      data.table(
        plural = metadata$plural,
        plural_index = plural_index,
        range = paste0("for n where 'plural' resolves to ", plural_index)
      )
    )
    setkeyv(PLURAL_RANGE_STRINGS, c("plural", "plural_index"))
    lockBinding("PLURAL_RANGE_STRINGS", asNamespace("potools"))
  }
  message("Thanks! Please file an issue on GitHub to get this language recognized permanently")
}
