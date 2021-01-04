function(x) {
  warning("I warned you!!")
  x+1
}

function(x) {
  stop("I really wish you'd reconsider")
}

function(x) {
  cat(sprintf(
    ngettext(length(x), "SOMEWHAT EPIC FAIL\n", "MAJORLY EPIC FAIL\n")
  ))
}
