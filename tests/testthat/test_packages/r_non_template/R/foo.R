a <- function(x) {
	warning("I warned you!")
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
  stop("You failed ", length(x), " times.")
}
