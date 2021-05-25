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
