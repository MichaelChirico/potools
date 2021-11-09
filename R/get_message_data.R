#' Extract user-visible messages from a package
#'
#' This function looks in the R and src directories of a package for
#' user-visible messages and compiles them as a
#' [data.table::data.table()] to facilitate
#' analyzing this corpus as such.
#'
#'
#' @param dir Character, default the present directory; a directory in which an
#' R package is stored.
#' @param custom_translation_functions A `list` with either/both of two
#' components, `R` and `src`, together governing how to extract any
#' non-standard strings from the package.
#'
#' See Details in [`translate_package()`][translate_package].
#' @param style Translation style, either `"base"` or `"explict"`.
#' The default, `NULL`, reads from the `DESCRIPTION` field
#' `Config/potools/style` so you can specify the style once for your
#' package.
#'
#' Both styles extract strings explicitly flagged for translation with
#' `gettext()` or `ngettext()`. The base style additionally extracts
#' strings in calls to `stop()`, `warning()`, and `message()`,
#' and to `stopf()`, `warningf()`, and `messagef()` if you have
#' added those helpers to your package. The explicit style also accepts
#' `tr_()` as a short hand for `gettext()`. See
#' `vignette("developer")` for more details.
#' @param verbose Logical, default `TRUE` (except during testing). Should
#' extra information about progress, etc. be reported?
#' @return
#' A `data.table` with the following schema:
#'
#' * `message_source`: `character`, either `"R"` or `"src"`,
#'   saying whether the string was found in the R or the src folder of the
#'   package
#' * `type`: `character`, either `"singular"` or
#'   `"plural"`; `"plural"` means the string came from
#'   [`ngettext()`][ngettext] and can be pluralized
#' * `file`: `character`, the file where the string was found
#' * `msgid`: `character`, the string (character literal or `char` array as
#'   found in the source); missing for all `type == "plural"` strings
#' * `msgid_plural`: `list(character, character)`, the strings
#'   (character literals or `char` arrays as found in the source); the first
#'   applies in English for `n=1` (see `ngettext`), while the second
#'   applies for `n!=1`; missing for all `type == "singular"` strings
#' * `call`: `character`, the full call containing the string
#'   that was found
#' * `line_number`: `integer`, the line in `file` where the string was found
#' * `is_repeat`: `logical`, whether the `msgid` is a duplicate within this
#'   `message_source`
#' * `is_marked_for_translation`:`logical`, whether the string is marked for
#'   translation (e.g., in R, all character literals supplied to a `...` argument in
#'   [`stop()`][stop] are so marked)
#' * `is_templated`, `logical`, whether the string is templatable (e.g., uses
#'   `%s` or other formatting markers)
#' @author Michael Chirico
#' @seealso [`translate_package()`][translate_package],
#' [`write_po_file()`][write_po_file]
#' @examples
#'
#' pkg <- system.file('pkg', package = 'potools')
#' get_message_data(pkg)
#'
#' # includes strings provided to the custom R wrapper function catf()
#' get_message_data(pkg, custom_translation_functions = list(R = "catf:fmt|1"))
#'
#' # includes untranslated strings provided to the custom
#' #   C/C++ wrapper function ReverseTemplateMessage()
#' get_message_data(
#'   pkg,
#'   custom_translation_functions = list(src = "ReverseTemplateMessage:2")
#' )
#'
#' # cleanup
#' rm(pkg)
#'
#' @export
get_message_data = function(
  dir = ".",
  custom_translation_functions = list(R = NULL, src = NULL),
  style = NULL,
  verbose = !is_testing()
) {
  package = get_desc_data(dir, 'Package')
  is_base = package == 'base'

  # If style not specified, read from DESCRIPTION
  if (is.null(style)) {
    style <- get_desc_data(dir, "Config/potools/style")
    if (is.na(style)) {
      style <- "base"
    }
  }

  if (verbose && dir.exists(file.path(dir, "R"))) message('Getting R-level messages...')
  r_message_data = get_r_messages(
    dir,
    custom_translation_functions = custom_translation_functions$R,
    style = style,
    is_base = is_base
  )

  if (verbose && dir.exists(file.path(dir, "src"))) message('Getting src-level messages...')
  src_message_data = get_src_messages(
    dir,
    custom_translation_functions = custom_translation_functions$src,
    is_base
  )

  rbind(
    R = r_message_data,
    src = src_message_data,
    fill = TRUE,
    idcol = "message_source"
  )
}
