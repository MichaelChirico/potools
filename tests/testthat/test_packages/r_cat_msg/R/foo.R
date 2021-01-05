function(x) {
  base::cat("I warned you!")
  x+1
}

function(x) {
  cat("Oh no", "you don't!")
}

function(x) {
  cat("Hi there", file=tempfile())
}

function(x) {
  cat(gettextf("shouldn't be translated"))
  cat("This costs", x, "dollars")
}
