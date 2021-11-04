# split off from tools::update_pkg_po() to only run the msgmerge & checkPoFile steps

# https://www.gnu.org/software/gettext/manual/html_node/msgmerge-Invocation.html
run_msgmerge = function(po_file, pot_file, previous = FALSE) {
  cmd <- paste("msgmerge",
    "--update",
    if (previous) "--previous", # show previous match for fuzzy matches
    shQuote(path.expand(po_file)),
    shQuote(path.expand(pot_file))
  )

  if (system(cmd) != 0L) {
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

update_mo_files = function(dir, package, verbose) {
  inst_dir <- file.path(dir, "inst", "po")
  lang_regex <- "^(R-)?([a-zA-Z_]+)\\.po$"

  po_files <- list.files(file.path(dir, "po"), pattern = "\\.po$")
  languages <- gsub(lang_regex, "\\2", po_files)
  mo_files <- gsub(lang_regex, sprintf("\\1%s.mo", package), po_files)
  mo_dirs <- file.path(inst_dir, languages, "LC_MESSAGES")
  # NB: dir.create() only accepts one directory at a time...
  for (mo_dir in unique(mo_dirs)) dir.create(mo_dir, recursive = TRUE, showWarnings = FALSE)

  for (ii in seq_along(po_files)) {
    run_msgfmt(
      po_file = file.path(dir, "po", po_files[ii]),
      mo_file = file.path(mo_dirs[ii], mo_files[ii]),
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



#' Create or update a `.po` file containing translations
#'
#' @description
#' * `po_create()` creates a new `po/{lang}.po` containing the messages to be
#'   translated.
#' * `po_update()` updates an existing `.po` file after the messages in a
#'   package have changed. The translations for existing messages are preserved;
#'   new messages are added; and translations for deleted message are marked
#'   as deprecated and moved to the bottom of the file.
#'
#' @param lang Language identifiers. These are typically two letters (e.g.
#'   "en" = English, "fr" = French, "es" = Spanish, "zh" = Chinese), but
#'   can include an additional suffix for languages that have regional
#'   variations (e.g. "fr_CN" = French Canadian, "zh_CN" = simplified
#'   characters as used in mainland China, "zh_TW" = traditional characters
#'   as used in Taiwan.)
#' @param dir Path to package root.
#' @param verbose If `TRUE`, explain what's happening.
po_create <- function(lang, dir = ".", verbose = TRUE) {
  package <- get_desc_data(dir)[["Package"]]
  po_path <- po_path(dir, lang)
  if (file.exists(po_path)) {
    po_update(lang, dir = dir, verbose = verbose)
    return(invisible())
  }

  if (verbose) messagef("Adding new %s translation", lang)
  run_msginit(pot_path(dir, package), po_path, lang)
}

#' @rdname po_create
po_update <- function(lang, dir = ".", verbose = TRUE) {
  package <- get_desc_data(dir)[["Package"]]

  if (verbose) messagef("Updating existing %s translation", lang)
  run_msgmerge(po_path(dir, lang), pot_path(dir, package), previous = TRUE)
}

po_path <- function(dir, lang) {
  file.path(dir, "po", paste0("R-", lang, ".po"))
}
pot_path <- function(dir, package) {
  file.path(dir, "po", paste0("R-", package, ".pot"))
}

# https://www.gnu.org/software/gettext/manual/html_node/msginit-Invocation.html
run_msginit <- function(pot_path, po_path, locale, width = 80) {
  cmd <- paste("msginit",
    "-i", shQuote(path.expand(pot_path)),
    "-o", shQuote(path.expand(po_path)),
    "-l", shQuote(locale),
    "-w", width,
    "--no-translator" # don't consult user-email etc
  )
  if (system(cmd) != 0L) {
    stopf("running msginit on '%s' failed", pot_path)
  }
  return(invisible())
}

