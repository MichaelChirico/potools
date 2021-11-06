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
#' into the binary `.mo` files that are installed with packages and used for
#' live translations.
#'
#' @param dir Path to package root directory.
#' @param package Name of package. If not supplied, read from `DESCRIPTION`.
#' @param lazy If `TRUE`, only `.mo` functions that are older than `.po`
#'   files be updated
#' @param verbose If `TRUE`, print information as it goes.
po_compile = function(dir = ".", package = NULL, lazy = TRUE, verbose = TRUE) {
  po_metadata <- get_po_metadata(dir = dir, package = package, lazy = lazy)
  dir_create(dirname(po_metadata$mo))

  if (!lazy) {
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
    if (verbose) messagef("Recompiling %s translation", po_metadata$language[ii])
    run_msgfmt(
      po_file = po_metadata$po[ii],
      mo_file = po_metadata$mo[ii],
      verbose = verbose
    )
  }

  return(invisible())
}

get_po_metadata <- function(dir = ".", package = NULL, lazy = TRUE) {
  if (is.null(package)) {
    package <- get_desc_data(dir)[["Package"]]
  }

  po_paths <- list.files(file.path(dir, "po"), pattern = "\\.po$", full.names = TRUE)

  lang_regex <- "^(R-)?([a-z]{2}(?:_[A-Z]{2})?)\\.po$"
  languages <- gsub(lang_regex, "\\2", basename(po_paths))
  mo_names <- gsub(lang_regex, sprintf("\\1%s.mo", package), basename(po_paths))
  mo_paths <- file.path(dir, "inst", "po", languages, "LC_MESSAGES", mo_names)

  out <- data.table(
    language = languages,
    po = po_paths,
    mo = mo_paths
  )

  if (lazy) {
    out <- out[is_outdated(po, mo)]
  }

  out
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
