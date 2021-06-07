function(x) {
  base::cat("I warned you!", fill=TRUE, append=TRUE) # (1)
  x+1
}

function(x) {
  # check that the newline is shown as "\n" (i.e., not a line return), #57
  cat("Oh no", "you\ndon't!") # (2)
  sep = ""
  cat("Miss me", "with", "this", sep=sep) # <unmarked: don't know sep statically>
}

function(x) {
  cat("Hi there", file=tempfile()) # <unmarked: don't translate cat to file>
  cat("Hi", "boss", sep="xx") # (3)
}

function(x) {
  cat(gettextf("shouldn't be translated"))  # <cat is unmarked: gettextf is done
  cat(gettext("me neither"), "\n")          # <cat is unmarked: gettext is done. #66>
  cat(foo(bar(ngettext(5, "a", "b"))), "c") # <cat is unmarked: as above, with further nesting>
  cat("This costs", x, "dollars")           # (4)
}
