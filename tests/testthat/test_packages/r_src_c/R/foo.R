f <- function(x) {
  # coverage: check_untranslated_cat skips this because of the second argument
  #   (even though it's marked untranslated, still the intent is shown that the
  #   author was translation-aware when writing). this lone cat in the package
  #   passes all but the last filter in check_untranslated_cat.
  cat("a message", gettext("but this one's in gettext", domain=NA))
  message("a string")
  x+1
}
