show_diagnostic_results <- function(results, diagnostic) {
  messagef('Found %d %s:', nrow(results), attr(diagnostic, "diagnostic_tag"))

  for (ii in seq_len(nrow(results))) {
    results[ii, cat(gettextf(
      '\nProblematic call:\n%s\n< File:%s, Line:%s >\n%s',
      call_color(call),
      file_color(file),
      file_color(line_number),
      if (is.na(replacement)) '' else gettextf('Potential replacement:\n%s\n', replacement_color(replacement))
    ))]
  }
}

diagnostic_schema = function() {
  data.table(
    call = character(),
    file = character(),
    line_number = integer(),
    replacement = character()
  )
}
