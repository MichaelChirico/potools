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
  cat(gettextf("shouldn't be translated")) # <cat is unmarked: gettextf is done
  cat("This costs", x, "dollars") # (4)
}
