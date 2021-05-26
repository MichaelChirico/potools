function(x) {
  base::cat("I warned you!", fill=TRUE, append=TRUE)
  x+1
}

function(x) {
  # check that the newline is shown as "\n" (i.e., not a line return), #57
  cat("Oh no", "you\ndon't!")
  sep = ""
  cat("Miss me", "with", "this", sep=sep)
}

function(x) {
  cat("Hi there", file=tempfile())
  cat("Hi", "boss", sep="xx")
}

function(x) {
  cat(gettextf("shouldn't be translated"))
  cat("This costs", x, "dollars")
}
