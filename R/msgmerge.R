# split off from tools::update_pkg_po() to only run the msgmerge & checkPoFile steps
run_msgmerge = function(po_file, pot_file) {
  if (system(sprintf("msgmerge --update %s %s", po_file, shQuote(pot_file))) != 0L) {
    # nocov these warnings? i don't know how to trigger them as of this writing.
    warningf("Running msgmerge on '%s' failed.", po_file)
  }

  res <- tools::checkPoFile(po_file, strictPlural = TRUE)
  if (nrow(res)) {
    warningf("tools::checkPoFile() found some issues in %s", po_file)
    print(res)
  }
  return(invisible())
}

run_msgfmt = function(po_file, mo_file, verbose) {
  use_stats <- if (verbose) '--statistics' else ''

  po_file <- path.expand(po_file)
  mo_file <- path.expand(mo_file)

  # See #218. Solaris msgfmt doesn't support -c or --statistics
  if (Sys.info()[["sysname"]] == "SunOS") {
    cmd = sprintf("msgfmt -o %s %s", shQuote(mo_file), shQuote(po_file)) # nocov
  } else {
    cmd = sprintf("msgfmt -c %s -o %s %s", use_stats, shQuote(mo_file), shQuote(po_file))
  }
  if (system(cmd) != 0L) {
    warningf(
      "running msgfmt on %s failed.\nHere is the po file:\n%s",
      basename(po_file), paste(readLines(po_file), collapse = "\n"),
      immediate. = TRUE
    )
  }
  return(invisible())
}

#' Compile `.po` files to `.mo`
#'
#' This function compiles the plain text `.po` files that translators work with
#' in to the binary `.mo` files that are installed with packages and used for
#' live translations.
#'
#' @param dir Path to package root directory.
#' @param package Name of package. If not supplied, read from `DESCRIPTION`.
#' @param verbose If `TRUE`, print information as it goes.
#' @param lazy If `TRUE`, only `.mo` functions that are older than `.po`
#'   files be updated
po_compile = function(dir = ".", package = NULL, lazy = TRUE, verbose = TRUE) {
  if (is.null(package)) {
    package <- get_desc_data(dir)[["Package"]]
  }

  po_paths <- list.files(file.path(dir, "po"), pattern = "\\.po$", full.names = TRUE)

  lang_regex <- "^(R-)?([a-zA-Z_]+)\\.po$"
  languages <- gsub(lang_regex, "\\2", basename(po_paths))
  mo_names <- gsub(lang_regex, sprintf("\\1%s.mo", package), basename(po_paths))
  mo_paths <- file.path(dir, "inst", "po", languages, "LC_MESSAGES", mo_names)
  dir_create(dirname(mo_paths))

  if (lazy) {
    outdated <- is_outdated(po_paths, mo_paths)
    po_paths <- po_paths[outdated]
    mo_paths <- mo_paths[outdated]
    languages <- languages[outdated]
  }

  for (ii in seq_along(po_paths)) {
    if (verbose) message("Recompiling ", languages[ii], " translation")
    run_msgfmt(
      po_file = po_paths[ii],
      mo_file = mo_paths[ii],
      verbose = verbose
    )
  }

  return(invisible())
}

update_en_quot_mo_files <- function(dir, verbose) {
  pot_files <- list.files(file.path(dir, "po"), pattern = "\\.pot$", full.names = TRUE)
  mo_dir <- file.path(dir, "inst", "po", "en@quot", "LC_MESSAGES")
  dir.create(mo_dir, recursive = TRUE, showWarnings = FALSE)
  for (pot_file in pot_files) {
    po_file <- tempfile()
    # tools:::en_quote is blocked, but we still need it for now
    get("en_quote", envir=asNamespace("tools"))(pot_file, po_file)
    run_msgfmt(
      po_file = po_file,
      mo_file = file.path(mo_dir, gsub("\\.pot$", ".mo", basename(pot_file))),
      verbose = verbose
    )
    unlink(po_file)
  }
  return(invisible())
}
