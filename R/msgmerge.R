# split off from tools::update_pkg_po() to only run the msgmerge & checkPoFile steps
run_msgmerge = function(po_file, pot_file, verbose) {
  val = system2("msgmerge", c("--update", shQuote(po_file), shQuote(pot_file)), stdout = TRUE, stderr = TRUE)
  if (!identical(attr(val, "status", exact = TRUE), NULL)) {
    # nocov these warnings? i don't know how to trigger them as of this writing.
    warningf("Running msgmerge on './po/%s' failed:\n  %s", basename(po_file), paste(val, collapse = "\n"))
  } else if (verbose) {
    messagef("Running msgmerge on './po/%s' succeeded:\n  %s", basename(po_file), paste(val, collapse = "\n"))
  }

  res <- tools::checkPoFile(po_file, strictPlural = TRUE)
  if (nrow(res)) {
    warningf("tools::checkPoFile() found some issues in %s", po_file)
    print(res)
  }
  return(invisible())
}

run_msgfmt = function(po_file, mo_file, verbose) {
  # See #218. Solaris msgfmt (non-GNU on CRAN) doesn't support --check or --statistics;
  #   see also https://bugs.r-project.org/show_bug.cgi?id=18150
  args = character()
  if (is_gnu_gettext()) {
    args = c("--check", if (verbose) '--statistics')
  }
  val = system2("msgfmt", c(args, "-o", shQuote(mo_file), shQuote(po_file)), stdout = TRUE, stderr = TRUE)
  if (!identical(attr(val, "status", exact = TRUE), NULL)) {
    warningf(
      "running msgfmt on %s failed; output:\n  %s\nHere is the po file:\n%s",
      basename(po_file), paste(val, collapse = "\n"), paste(readLines(po_file), collapse = "\n"),
      immediate. = TRUE
    )
  } else if (verbose) {
    messagef(
      "running msgfmt on %s succeeded; output:\n  %s",
      basename(po_file), paste(val, collapse = "\n")
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
    # don't use tempfile() -- want a static basename() to keep verbose output non-random
    po_file <- file.path(tempdir(), if (startsWith(basename(pot_file), "R-")) "R-en@quot.po" else "en@quot.po")
    on.exit(unlink(po_file))
    # tools:::en_quote is blocked, but we still need it for now
    get("en_quote", envir=asNamespace("tools"))(pot_file, po_file)
    run_msgfmt(
      po_file = po_file,
      mo_file = file.path(mo_dir, gsub("\\.pot$", ".mo", basename(pot_file))),
      verbose = verbose
    )
  }
  return(invisible())
}
