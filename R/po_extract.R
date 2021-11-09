#' Extract messages for translation into a `.pot` file
#'
#' `po_extract()` scans your package for strings to be translated and
#' saves them into a `.pot` template file (in the package's `po`
#' directory). You should never modify this file by hand; instead modify the
#' underlying source code and re-run `po_extract()`.
#'
#'
#' @param dir Character, default the present directory; a directory in which an
#' R package is stored.
#' @param custom_translation_functions A `list` with either/both of two
#' components, `R` and `src`, together governing how to extract any
#' non-standard strings from the package.
#'
#' See Details in [`translate_package()`][translate_package].
#' @param verbose Logical, default `TRUE` (except during testing). Should
#' extra information about progress, etc. be reported?
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
#' @return The extracted messages as computed by
#' [`get_message_data()`][get_message_data], invisibly.
#' @export
po_extract <- function(
    dir = ".",
    custom_translation_functions = list(),
    verbose = !is_testing(),
    style = NULL) {

  message_data <- get_message_data(dir,
    custom_translation_functions = custom_translation_functions,
    verbose = verbose,
    style = style
  )

  n <- nrow(message_data)
  if (!n) {
    if (verbose) message('No messages to translate')
    return(invisible())
  }
  # TODO: messagef() is double-translating the ngettext() result...
  #   it needs an escape valve
  if (verbose) messagef(ngettext(n, "Found %i message", "Found %i messages"), n)

  po_dir <- file.path(dir, 'po')
  dir.create(po_dir, showWarnings = FALSE)

  desc <- get_desc_data(dir)
  po_params = list(
    package = desc[['Package']],
    version = desc[['Version']],
    copyright = NULL,
    bugs = NULL
  )

  write_po_files(message_data, po_dir, po_params, template = TRUE, verbose = verbose)
  invisible(message_data)
}
