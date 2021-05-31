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

run_msgfmt = function() {

}
