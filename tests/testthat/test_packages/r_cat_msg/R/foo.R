function(x) {
  base::cat("I warned you!", fill=TRUE, append=TRUE)
  x+1
}

function(x) {
  cat("Oh no", "you don't!")
}

function(x) {
  cat("Hi there", file=tempfile())
  cat("Hi", "boss", sep="xx")
}

function(x) {
  cat(gettextf("shouldn't be translated"))
  cat("This costs", x, "dollars")
}
