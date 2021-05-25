a <- function(x) {
	warning("I warned you!", immediate. = TRUE)
	x+1
}

b <- function(x) {
	stop("You failed ", length(x), " times.")
}

c <- function(x) {
	cat(sprintf(
		ngettext(length(x), "small fail\n", "big fail\n")
	))
}

# add some duplication of the messages to stress test logic
#   on handling duplicates; part of #49
d <- function(x) {
  stop("I warned you!")
}

e <- function(x) {
  # do recommend gettextf here: "Argument missing: %s" is better. #51
  warning("Argument missing: ", x)
  # don't recommend gettextf here: single call
  warning(strrep("abcdefg", 10L), call. = FALSE)
  stop("You failed ", length(x), " times.")
}
