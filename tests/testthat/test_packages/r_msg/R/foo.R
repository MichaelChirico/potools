f1 <- function(x) {
  base::warning("I warned you!")
  x+1
}

f2 <- function(x) {
  stop("Oh no you don't!")
}

f3 <- function(x) {
  gettext("Hi there")
  stop("Oh no you don't!")
}

f4 <- function(x) {
  # include cat() in the string here for coverage of
  #   check_untranslated_cat, which does a first pass looking for
  #   cat() anywhere in the call, then filters for cat actually
  #   being a call; this passes the first filter, not the second.
  stop(domain=NA, gettextf(
    fmt = "Avg cat() failures: %.02f; N failures: %d; failure: %s",
    mean(x), length(x), "don't translate me"
  ))
}

f5 <- function(x) {
  # source markers for skipping translation.
  # notranslate for single lines
  message("skip me for translation") # notranslate

  # # notranslate start / # notranslate end for ranges.
  # ^ also testing that within-line "range" doesn't break things
  # notranslate start
  message("me too")
  message("me three")
  # notranslate end
}
