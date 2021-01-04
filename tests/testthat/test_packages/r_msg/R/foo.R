function(x) {
  warning("I warned you!")
  x+1
}

function(x) {
  stop("Oh no you don't!")
}

function(x) {
  gettext("Hi there")
}

function(x) {
  stop(domain=NA, gettextf(
    "Avg failures: %.02f; N failures: %d; failure: %s",
    mean(x), length(x), deparse1(substitute(x))
  ))
}
