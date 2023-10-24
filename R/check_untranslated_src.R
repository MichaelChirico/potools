# check a package for character arrays in messaging functions that
#   haven't been passed through the translation macro


#' Check for cracked messages in C/C++ sources
#'
#' Diagnose the C/C++ messages in a package to discover untranslated messages
#'
#'
#' This diagnostic looks for literal `char` arrays passed to messaging
#' functions (as identified by [translate_package()]) which are not
#' marked for translation (by tagging them for translation with `_` or
#' `N_` macros). These strings cannot be translated until they are so
#' marked.
#'
#' @param message_data A `data.table`, or object convertible to one.
#' @return A `data.table` with columns `call`, `file`,
#' `line_number`, and `replacement` summarizing the results.
#' `replacement` is `NA` at this time, i.e., no replacement is
#' provided.
#' @author Michael Chirico
#' @seealso [translate_package()], [update_pkg_po()]
#' @examples
#'
#' pkg <- file.path(system.file(package = 'potools'), 'pkg')
#' # copy to a temporary location to be able to read/write/update below
#' tmp_pkg <- file.path(tempdir(), "pkg")
#' dir.create(tmp_pkg)
#' file.copy(pkg, dirname(tmp_pkg), recursive = TRUE)
#'
#' # first, extract message data
#' message_data = get_message_data(
#'   tmp_pkg,
#'   custom_translation_functions = list(src = "ReverseTemplateMessage:2")
#' )
#'
#' # now, diagnose the messages for any untranslated messages in C/C++
#' check_untranslated_src(message_data)
#'
#' # cleanup
#' unlink(tmp_pkg, recursive = TRUE)
#' rm(pkg, tmp_pkg, message_data)
#' @export
check_untranslated_src <- function(message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)

  return(message_data[
    message_source == "src" & !is_marked_for_translation,
    .(call, file, line_number, replacement = NA_character_)
  ])
}
attr(check_untranslated_src, "diagnostic_tag") <- "src messaging calls that were not properly marked for translation"
