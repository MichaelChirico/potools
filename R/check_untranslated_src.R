# check a package for character arrays in messaging functions that
#   haven't been passed through the translation macro
check_untranslated_src <- function (message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)

  return(message_data[
    message_source == "src" & !is_marked_for_translation,
    .(call, file, line_number, replacement = NA_character_)
  ])
}
attr(check_untranslated_src, "diagnostic_tag") <- "src messaging calls that were not properly marked for translation"
