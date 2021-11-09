#' Extract user-visible messages from a package
#' 
#' This function looks in the R and src directories of a package for
#' user-visible messages and compiles them as a
#' \code{\link[data.table:data.table]{data.table::data.table()}} to facilitate
#' analyzing this corpus as such.
#' 
#' 
#' @param dir Character, default the present directory; a directory in which an
#' R package is stored.
#' @param custom_translation_functions A \code{list} with either/both of two
#' components, \code{R} and \code{src}, together governing how to extract any
#' non-standard strings from the package.
#' 
#' See Details in \code{\link[=translate_package]{translate_package()}}.
#' @param style Translation style, either \code{"base"} or \code{"explict"}.
#' The default, \code{NULL}, reads from the \code{DESCRIPTION} field
#' \code{Config/potools/style} so you can specify the style once for your
#' package.
#' 
#' Both styles extract strings explicitly flagged for translation with
#' \code{gettext()} or \code{ngettext()}. The base style additionally extracts
#' strings in calls to \code{stop()}, \code{warning()}, and \code{message()},
#' and to \code{stopf()}, \code{warningf()}, and \code{messagef()} if you have
#' added those helpers to your package. The explicit style also accepts
#' \code{tr_()} as a short hand for \code{gettext()}. See
#' \code{vignette("developer")} for more details.
#' @param verbose Logical, default \code{TRUE} (except during testing). Should
#' extra information about progress, etc. be reported?
#' @return A \code{data.table} with the following schema: \itemize{ \item
#' \code{message_source}, \code{character}, either \code{"R"} or \code{"src"},
#' saying whether the string was found in the R or the src folder of the
#' package \item \code{type}, \code{character}, either \code{"singular"} or
#' \code{"plural"}; \code{"plural"} means the string came from
#' \code{\link[=ngettext]{ngettext()}} and can be pluralized \item \code{file},
#' \code{character}, the file where the string was found \item \code{msgid},
#' \code{character}, the string (character literal or \code{char} array as
#' found in the source); missing for all \code{type == "plural"} strings \item
#' \code{msgid_plural}, \code{list(character, character)}, the strings
#' (character literals or \code{char} arrays as found in the source); the first
#' applies in English for \code{n=1} (see \code{ngettext}), while the second
#' applies for \code{n!=1}; missing for all \code{type == "singular"} strings
#' \item \code{call}, \code{character}, the full call containing the string
#' that was found \item \code{line_number}, \code{integer}, the line in
#' \code{file} where the string was found \item \code{is_repeat},
#' \code{logical}, whether the \code{msgid} is a duplicate within this
#' \code{message_source} \item \code{is_marked_for_translation},
#' \code{logical}, whether the string is marked for translation (e.g., in R,
#' all character literals supplied to a \code{...} argument in
#' \code{\link[=stop]{stop()}} are so marked) \item \code{is_templated},
#' \code{logical},whether the string is templatable (e.g., uses \verb{%s} or
#' other formatting markers) }
#' @author Michael Chirico
#' @seealso \code{\link[=translate_package]{translate_package()}},
#' \code{\link[=write_po_file]{write_po_file()}}
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
#' @export get_message_data
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
