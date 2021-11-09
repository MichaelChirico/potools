#' Create a new `.po` file
#'
#' `po_create()` creates a new `po/{languages}.po` containing the messages to be
#' translated. If a translation already exists, it'll be updated with any
#' changes to the `.pot` since it was last touched.
#'
#' @param languages Language identifiers. These are typically two letters (e.g.
#'   "en" = English, "fr" = French, "es" = Spanish, "zh" = Chinese), but
#'   can include an additional suffix for languages that have regional
#'   variations (e.g. "fr_CN" = French Canadian, "zh_CN" = simplified
#'   characters as used in mainland China, "zh_TW" = traditional characters
#'   as used in Taiwan.)
#' @inheritParams po_extract
#' @seealso [po_update()] to update all `.po` files with changes from the
#'   `.pot`.
po_create <- function(languages, dir = ".", verbose = !is_testing()) {
  package <- get_desc_data(dir, "Package")
  po_files <- po_language_files(languages, dir)

  for (ii in seq_along(nrow(po_files))) {
    row <- po_files[ii]
    if (file.exists(row$po_path)) {
      if (verbose) messagef("Updating '%s' %s translation", row$language, row$type)
      run_msgmerge(row$po_path, row$pot_path, previous = TRUE, verbose = verbose)
    } else {
      if (verbose) messagef("Creating '%s' %s translation", row$language, row$type)
      run_msginit(row$po_path, row$pot_path, locale = row$language, verbose = verbose)
    }
  }

  invisible(po_files)
}

po_language_files <- function(languages, dir = ".") {
  po_files <- data.table::CJ(type = pot_types(dir), language = languages)
  po_files[, "po_path" := file.path(dir, "po", paste0(po_prefix(po_files$type), po_files$language, ".po"))]
  po_files[, "pot_path" := pot_paths(dir, po_files$type)]
  po_files[]
}

pot_paths <- function(dir, type, package = NULL) {
  if (is.null(package)) {
    package <- get_desc_data(dir, "Package")
  }
  file.path(dir, "po", paste0(po_prefix(type), package, ".pot"))
}
po_prefix <- function(type = c("R", "src")) {
  ifelse(type == "R", "R-", "")
}
pot_types <- function(dir = ".") {
  types <- c("R", "src")
  types[file.exists(pot_paths(dir, types))]
}
