# check a package for character arrays in messaging functions that
#   haven't been passed through the translation macro
# TODO: this is very close to check_untranslated_cat -- the major difference is that
#   check_untranslated_cat offers a suggested translated alternative. Is it possible
#   to merge the logic for these two diagnostics?
check_untranslated_src <- function (message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)

  return(message_data[message_source == "src" & !is_marked_for_translation])
}
attr(check_untranslated_src, "diagnostic_tag") <- "src messaging calls that were not properly marked for translation"
