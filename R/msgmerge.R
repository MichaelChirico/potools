# split off from tools::update_pkg_po() to only run the msgmerge & checkPoFile steps
run_msgmerge = function(po_file, pot_file) {
  if (system(sprintf("msgmerge --update %s %s", po_file, shQuote(pot_file))) != 0L) {
    warning(domain = NA, gettextf("Running msgmerge on '%s' failed.", po_file))
  }

  res <- checkPoFile(po_file, strictPlural = TRUE)
  if (nrow(res)) {
    warning(domain = NA, gettextf("tools::checkPoFile() found some issues in %s", po_file))
    print(res)
  }
  return(invisible())
}

run_msgfmt = function(dir, package) {
  browser()
  inst_dir <- file.path(dir, "inst", "po")
  lang_regex <- "^(R-)?([a-zA-Z_]+)\\.po$"

  po_files <- list.files(file.path(dir, "po"), pattern = "\\.po$")
  languages <- gsub(lang_regex, "\\2", po_files)
  mo_files <- gsub(lang_regex, sprintf("\\1%s.mo", package), po_files)

  for (ii in seq_along(po_files)) {
    mo_dir <- file.path(inst_dir, languages[ii], "LC_MESSAGES")
    dir.create(mo_dir, recursive = TRUE, showWarnings = FALSE)

    po_file <- file.path(dir, "po", po_files[ii])
    mo_file <- file.path(mo_dir, mo_files[ii])
    if (system(sprintf("msgfmt -c --statistics -o %s %s", shQuote(mo_file), shQuote(po_file))) != 0L) {
      warning(domain = NA, gettextf("running msgfmt on %s failed", basename(po_file)), immediate. = TRUE)
    }
  }

  r_mo_file <- file.path(mo_dir, sprintf("R-%s.mo", package))


  # on UTF-8 machines we install the en@quot messages too
  # TODO: streamline this -- en_quote is definitely doing some redundant stuff
  if (l10n_info()[["UTF-8"]]) {
    lang <- "en@quot"
    message("  R-", lang, ":", domain = NA)
    f <- tempfile()
    tools:::en_quote(potfile, f)
    dest <- file.path(stem, lang, "LC_MESSAGES")
    dir.create(dest, FALSE, TRUE)
    dest <- file.path(dest, sprintf("R-%s.mo", pkg))
    cmd <- paste("msgfmt -c --statistics -o", shQuote(dest),
        shQuote(f))
    if (system(cmd) != 0L)
        warning(sprintf("running msgfmt on %s failed", basename(f)),
            domain = NA, immediate. = TRUE)
  }
}
