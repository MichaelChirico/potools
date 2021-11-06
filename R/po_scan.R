po_scan <- function(dir = ".", custom_translation_functions = list(), verbose = TRUE, include_conditions = TRUE) {
  message_data <- get_message_data(dir,
    custom_translation_functions = custom_translation_functions,
    verbose = verbose,
    include_conditions = include_conditions
  )
  if (!nrow(message_data)) {
    if (verbose) message('No messages to translate')
    return(invisible())
  }

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
  invisible()
}
