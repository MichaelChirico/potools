a <- function(x) {
  # make sure to skip the immediate. argument when reporting
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
  # coverage: check_untranslated_cat skips this because of the second argument.
  #   this lone cat in the package passes all but the last filter.
  cat("a message", gettext("but this one's translated"))
  stop("I warned you!")
}

e <- function(x) {
  # do recommend gettextf here: "Argument missing: %s" is better. #51
  warning(#also don't fail due to intervening comments, #59
    "Argument missing: ", # see #59; kept comment tight above to test numbering exactly
    x
  )
  # don't recommend gettextf here: single call
  warning(strrep("abcdefg", 10L), call. = FALSE)
  stop("You failed ", length(x), " times.")
}
