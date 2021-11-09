#' Update all `.po` files with changes in `.pot`
#'
#' `po_update()` updates existing `.po` file after the `.pot` file has changed.
#' The translations for existing messages are preserved; new messages are added;
#' and translations for deleted message are marked as deprecated and moved to
#' the bottom of the file.
#'
#' @inheritParams po_extract
#' @param lazy If `TRUE`, only `.po` files that are older than their
#'   corresponding `.pot` file will be updated
#' @rdname po_create
po_update <- function(dir = ".", lazy = TRUE, verbose = !is_testing()) {
  meta <- get_po_metadata(dir)
  if (lazy) {
    meta <- meta[is_outdated(meta$po, meta$pot)]
  }

  for (ii in seq_len(nrow(meta))) {
    row <- meta[ii]
    if (verbose) messagef("Updating '%s' %s translation", row$language, row$type)
    run_msgmerge(row$po, row$pot, previous = TRUE)
  }

  invisible(meta)
}
