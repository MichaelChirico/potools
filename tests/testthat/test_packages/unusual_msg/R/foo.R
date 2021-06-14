f1 <- function(x) {
  base::warning("I warned you!")
  # testing nested strings parse correctly -- after peeling away raw strings,
  #   these look like "normal" quote-wrapped strings. don't remove those quotes.
  #   similarly for the reverse situation next.
  message(r"('abc')")
  message(R'("def")')
  message("R('abc')")
  message('r("def")')
  message(R'---[ghi]---')
}

f2 <- function(x) {
  # empty string doesn't show up in .pot
  stop("")
}

f3 <- function(x) {
  # nested functions are excluded/not double-counted
  warning(gettext("Hi there"))
  # trimws() is done here
  message(gettextf("good %s ", "grief"))
  # trimws() is not done here
  stop(sprintf(ngettext(
    10,
    "singular ",
    "plural "
  )))
}

f4 <- function(x) {
  # don't write untranslated strings to .pot
  cat("i am not in .pot")
  # multiple strings in the same expression are ordered properly
  warning(
    '"first"', "second", "third",
    "fourth", "fifth", "sixth"
  )
}

f5 <- function(y) {
  # encoding/unencoding works as intended
  message("\\n vs \n is OK")
  message("\\t vs \t is OK")
  message('strings with "quotes" are OK')
  message("strings with escaped \"quotes\" are OK")
}
