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

run_msgfmt = function(po_file, mo_file, verbose) {
  use_stats <- if (verbose) '--statistics' else ''
  if (system(sprintf("msgfmt -c %s -o %s %s", use_stats, shQuote(mo_file), shQuote(po_file))) != 0L) {
    warning(domain = NA, gettextf("running msgfmt on %s failed", basename(po_file)), immediate. = TRUE)
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
  dir.create(unique(mo_dirs), recursive = TRUE, showWarnings = FALSE)

  for (ii in seq_along(po_files)) {
    run_msgfmt(
      po_file = file.path(dir, "po", po_files[ii]),
      mo_file = file.path(mo_dirs[ii], mo_files[ii]),
      verbose = verbose
    )
  }

  # on UTF-8 machines we install the en@quot messages too
  # TODO: streamline this -- en_quote is definitely doing some redundant stuff
  if (l10n_info()[["UTF-8"]]) {
    pot_files <- list.files(file.path(dir, "po"), pattern = "\\.pot$", full.names = TRUE)
    mo_dir <- file.path(inst_dir, "en@quot", "LC_MESSAGES")
    dir.create(mo_dir, recursive = TRUE, showWarnings = FALSE)
    for (pot_file in pot_files) {
      po_file <- tempfile()
      tools:::en_quote(pot_file, po_file)
      run_msgfmt(
        po_file = po_file,
        mo_file = file.path(inst_dir, gsub("\\.pot$", ".mo", basename(pot_file))),
        verbose = verbose
      )
    }
  }
}
