function(x) {
  warning("I warned you!!")
  x+1
}

function(x) {
  stop("I really wish you'd reconsider")
}

function(x) {
  cat(sprintf(
    ngettext(length(x), "small fail\n", "big fail\n")
  ))
}
