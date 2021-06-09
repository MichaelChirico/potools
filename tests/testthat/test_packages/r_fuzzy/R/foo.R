a <- function(x) {
  warning("I warned you!!")
  x+1
}

b <- function(x) {
  stop("I really wish you'd reconsider")
}

c <- function(x) {
  cat(sprintf(
    ngettext(length(x), "SOMEWHAT EPIC FAIL", "MAJORLY EPIC FAIL")
  ))
  # coverage test: only cat() call in this package, and it gets skipped because of file=
  cat("hello", file = tempfile())
}
