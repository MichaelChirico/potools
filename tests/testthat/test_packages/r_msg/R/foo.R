function(x) {
  warning("I warned you!")
  x+1
}

function(x) {
  stop("Oh no you don't!")
}

function(x) {
  cat(sprintf(
    ngettext(length(x), "small fail\n", "big fail\n")
  ))
}
