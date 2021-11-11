#' Extract messages for translation into a `.pot` file
#'
#' @description
#' `po_extract()` scans your package for strings to be translated and
#' saves them into a `.pot` template file (in the package's `po`
#' directory). You should never modify this file by hand; instead modify the
#' underlying source code and re-run `po_extract()`.
#'
#' If you have existing translations, call [po_update()] after [po_extract()]
#' to update them with the changes.
#'
#' @returns The extracted messages as computed by [get_message_data()],
#'   invisibly.
#' @inheritParams get_message_data
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
