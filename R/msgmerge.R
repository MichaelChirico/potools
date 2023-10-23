# split off from tools::update_pkg_po() to only run the msgmerge & checkPoFile steps

# https://www.gnu.org/software/gettext/manual/html_node/msgmerge-Invocation.html
# https://docs.oracle.com/cd/E36784_01/html/E36870/msgmerge-1.html#scrolltoc
run_msgmerge <- function(po_file, pot_file, previous = FALSE, verbose = TRUE) {
  check_potools_sys_reqs("msgmerge")
  args <- c(
    "--update", shQuote(path.expand(po_file)),
    "--backup=off",
    if (previous) "--previous", #show previous match for fuzzy matches
    shQuote(path.expand(pot_file))
  )

  if (verbose) {
    message("Running system command msgmerge ", paste(args, collapse = " "), "...")
  }
  val <- system2("msgmerge", args, stdout = TRUE, stderr = TRUE)
  if (!identical(attr(val, "status", exact = TRUE), NULL)) {
    # nocov these warnings? i don't know how to trigger them as of this writing.
    warningf("Running msgmerge on './po/%s' failed:\n  %s", basename(po_file), paste(val, collapse = "\n"))
  } else if (verbose) {
    messagef(paste(val, collapse = "\n"))
  }

  res <- tools::checkPoFile(po_file, strictPlural = TRUE)
  if (nrow(res)) {
    warningf("tools::checkPoFile() found some issues in %s", po_file)
    print(res)
  }
  return(invisible())
}

run_msgfmt = function(po_file, mo_file, verbose) {
  check_potools_sys_reqs("msgfmt")
  use_stats <- if (verbose) '--statistics' else ''

  po_file <- path.expand(po_file)
  mo_file <- path.expand(mo_file)

  # See #218, #221. Solaris msgfmt doesn't support -c or --statistics
  #   see also https://bugs.r-project.org/show_bug.cgi?id=18150
  if (is_gnu_gettext()) {
    cmd = glue("msgfmt -c {use_stats} -o {shQuote(mo_file)} {shQuote(po_file)}")
  } else {
    cmd = glue("msgfmt -o {shQuote(mo_file)} {shQuote(po_file)}") # nocov
  }
  if (verbose) {
    message("Running system command ", cmd, "...")
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

update_en_quot_mo_files <- function(dir, verbose) {
  check_potools_sys_reqs(c("msgfmt", "msginit", "msgconv"))
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

# https://www.gnu.org/software/gettext/manual/html_node/msginit-Invocation.html
# https://docs.oracle.com/cd/E36784_01/html/E36870/msginit-1.html#scrolltoc
run_msginit <- function(po_path, pot_path, locale, width = 80L, verbose = TRUE) {
  check_potools_sys_reqs("msginit")
  args <- c(
    "-i", shQuote(path.expand(pot_path)),
    "-o", shQuote(path.expand(po_path)),
    "-l", shQuote(locale),
    "-w", width,
    "--no-translator" # don't consult user-email etc
  )
  if (verbose) {
    message("Running system command msginit ", paste(args, collapse = " "), "...")
  }
  val <- system2("msginit", args, stdout = TRUE, stderr = TRUE)
  if (!identical(attr(val, "status", exact = TRUE), NULL)) {
    stopf("Running msginit on '%s' failed", pot_path)
  } else if (verbose) {
    messagef(paste(val, collapse = "\n"))
  }
  return(invisible())
}
