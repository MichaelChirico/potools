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

run_msgfmt = function(dir) {
  browser()
  inst_dir <- file.path(dir, "inst", "po")
  mo_dir <- file.path(inst_dir, languages, "LC_MESSAGES")
  dir.create(mo_dir, recursive = TRUE, showWarnings = FALSE)
  r_mo_file <- file.path(mo_dir, sprintf("R-%s.mo", package))
  if (system(sprintf("msgfmt -c --statistics -o %s %s", shQuote(r_mo_file), shQuote(po_file))) != 0L) {
    warning(domain = NA, gettextf("running msgfmt on %s failed", basename(po_file)), immediate. = TRUE)
  }

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
