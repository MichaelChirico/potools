f <- function(x) {
  # coverage: check_untranslated_cat skips this because of the second argument.
  #   this lone cat in the package passes all but the last filter.
  cat("a message", gettext("but this one's translated"))
  message("a string")
  x+1
}
