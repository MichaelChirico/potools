#' Extract messages for translation into a `.pot` file
#'
#' `po_extract()` scans your package for strings to be translated and
#' saves them in to `.pot` template file. You should never modify this
#' file by hand; instead modify the underlying source code and re-run
#' `po_extract()`.
#'
#' @returns The extracted messages as computed by [get_message_data()],
#'   invisibly.
po_extract <- function(
    dir = ".",
    custom_translation_functions = list(),
    verbose = TRUE,
    style = c("base", "explicit")) {

  style <- match.arg(style)

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
