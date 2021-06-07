show_diagnostic_results <- function(results, diagnostic) {
  message(domain=NA, gettextf('Found %d %s:', nrow(results), attr(diagnostic, "diagnostic_tag")))

  for (ii in seq_len(nrow(results))) {
    results[ii, cat(gettextf(
      '\nProblematic call:\n%s\n< File:%s, Line:%s >\nPotential replacement:\n%s\n',
      call_color(call),
      file_color(file),
      file_color(line_number)
    ))]
  }
}
