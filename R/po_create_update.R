
#' Create or update a `.po` file containing translations
#'
#' @description
#' * `po_create()` creates a new `po/{languages}.po` containing the messages to be
#'   translated.
#' * `po_update()` updates an existing `.po` file after the messages in a
#'   package have changed. The translations for existing messages are preserved;
#'   new messages are added; and translations for deleted message are marked
#'   as deprecated and moved to the bottom of the file.
#'
#' @param languages Language identifiers. These are typically two letters (e.g.
#'   "en" = English, "fr" = French, "es" = Spanish, "zh" = Chinese), but
#'   can include an additional suffix for languages that have regional
#'   variations (e.g. "fr_CN" = French Canadian, "zh_CN" = simplified
#'   characters as used in mainland China, "zh_TW" = traditional characters
#'   as used in Taiwan.)
#' @param dir Path to package root.
#' @param verbose If `TRUE`, explain what's happening.
po_create <- function(languages, dir = ".", verbose = TRUE) {
  package <- get_desc_data(dir)[["Package"]]
  po_path <- po_path(dir, languages)
  if (file.exists(po_path)) {
    po_update(languages, dir = dir, verbose = verbose)
    return(invisible())
  }

  if (verbose) messagef("Adding new %s translation", languages)
  run_msginit(pot_path(dir, package), po_path, languages)
}

#' @rdname po_create
po_update <- function(languages, dir = ".", verbose = TRUE) {
  package <- get_desc_data(dir)[["Package"]]

  if (verbose) messagef("Updating existing %s translation", languages)
  run_msgmerge(po_path(dir, languages), pot_path(dir, package), previous = TRUE)
}
