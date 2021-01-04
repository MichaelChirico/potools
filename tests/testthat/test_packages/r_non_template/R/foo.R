function(x) {
  warning("I warned you!")
  x+1
}

function(x) {
  stop("You failed ", length(x), " times.")
}

function(x) {
  cat(sprintf(
    ngettext(length(x), "small fail\n", "big fail\n")
  ))
}
