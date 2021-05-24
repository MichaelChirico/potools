f5 <- function(x) {
  cat(sprintf(
    ngettext(length(x), msg1 = "small fail\n", msg2 = "big fail\n")
  ))
}
