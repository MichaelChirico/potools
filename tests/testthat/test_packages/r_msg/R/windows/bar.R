f5 <- function(x) {
  cat(sprintf(
    ngettext(length(x), "small fail\n", "big fail\n")
  ))
}
