f5 <- function(x) {
  cat(sprintf(
    ngettext(length(x), msg1 = "small fail", msg2 = "big fail")
  ))
}
