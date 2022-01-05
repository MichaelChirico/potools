#' Update all `.po` files with changes in `.pot`
#'
#' @description
#' `po_update()` updates existing `.po` file after the `.pot` file has changed.
#' There are four cases:
#'
#' * New messages: added with blank `msgstr`.
#'
#' * Deleted messages: marked as deprecated and moved to the bottom of the file.
#'
#' * Major changes to existing messages: appear as an addition and a deletion.
#'
#' * Minor changes to existing messages: will be flagged as fuzzy.
#'
#'     ```
#'     #, fuzzy, c-format
#'     #| msgid "Generating en@quot translations"
#'     msgid "Updating '%s' %s translation"
#'     msgstr "memperbarui terjemahan bahasa en@quot..."
#'     ```
#'
#'     The previous message is given in comments starting with `#|`.
#'     Translators need to update the actual (uncommented) `msgstr` manually,
#'     using the old `msgid` as a potential reference, then
#'     delete the old translation and the `fuzzy` comment (c-format should
#'     remain, if present).
#'
#' @inheritParams po_extract
#' @param lazy If `TRUE`, only `.po` files that are older than their
#'   corresponding `.pot` file will be updated.
#' @export
po_update <- function(dir = ".", lazy = TRUE, verbose = !is_testing()) {
  meta <- get_po_metadata(dir)
  if (lazy) {
    meta <- meta[is_outdated(meta$pot, meta$po)]
  }

  for (ii in seq_len(nrow(meta))) {
    row <- meta[ii]
    if (verbose) messagef("Updating '%s' %s translation", row$language, row$type)
    run_msgmerge(row$po, row$pot, previous = TRUE, verbose = verbose)
  }

  invisible(meta)
}
