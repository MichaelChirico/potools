f1 <- function(x) {
  base::warning("I warned you!")
  x+1
}

f2 <- function(x) {
  stop("Oh no you don't!")
}

f3 <- function(x) {
  gettext("Hi there")
}

f4 <- function(x) {
  stop(domain=NA, gettextf(
    fmt = "Avg failures: %.02f; N failures: %d; failure: %s",
    mean(x), length(x), "don't translate me"
  ))
}
