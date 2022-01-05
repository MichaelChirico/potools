#' Compile `.po` files to `.mo`
#'
#' This function compiles the plain text `.po` files that translators work
#' with into the binary `.mo` files that are installed with packages and
#' used for live translations.
#'
#'
#' @param dir Path to package root directory.
#' @param package Name of package. If not supplied, read from
#' `DESCRIPTION`.
#' @param lazy If `TRUE`, only `.mo` functions that are older than
#' `.po` files be updated
#' @param verbose If `TRUE`, print information as it goes.
#' @export
po_compile = function(dir = ".", package = NULL, lazy = TRUE, verbose = TRUE) {
  po_metadata <- get_po_metadata(dir = dir, package = package)
  dir_create(dirname(po_metadata$mo))

  if (lazy) {
    po_metadata <- po_metadata[is_outdated(po_metadata$po, po_metadata$mo)]
  } else {
    # Clear out all older translations
    mo_dirs <- dir(file.path(dir, "inst", "po"), full.names = TRUE)
    to_delete <- mo_dirs[!basename(mo_dirs) %in% c(po_metadata$language, "en@quot")]

    for (dir in to_delete) {
      if (verbose) messagef(
        "Found a compiled translation for %s at %s, but no corresponding .po; deleting",
        basename(dir), dirname(dir)
      )
      unlink(dir, recursive = TRUE)
    }
  }

  for (ii in seq_len(nrow(po_metadata))) {
    row <- po_metadata[ii]
    if (verbose) messagef("Recompiling '%s' %s translation", row$language, row$type)
    run_msgfmt(row$po, row$mo, verbose = verbose)
  }

  return(invisible())
}


get_po_metadata <- function(dir = ".", package = NULL) {
  if (is.null(package)) {
    package <- get_desc_data(dir, "Package")
  }

  lang_regex <- "^(R-)?([a-z]{2}(?:_[A-Z]{2})?)\\.po$"
  po_paths <- list.files(file.path(dir, "po"), pattern = lang_regex, full.names = TRUE)

  languages <- gsub(lang_regex, "\\2", basename(po_paths))
  type <- ifelse(startsWith(basename(po_paths), "R-"), "R", "src")

  mo_names <- gsub(lang_regex, glue("\\1{package}.mo"), basename(po_paths))
  mo_paths <- file.path(dir, "inst", "po", languages, "LC_MESSAGES", mo_names)
  pot_paths <- pot_paths(dir, type, package = package)

  data.table(
    language = languages,
    type = type,
    po = po_paths,
    pot = pot_paths,
    mo = mo_paths
  )
}
