# tends to make some really repetitive messages with call.=TRUE, so always turn it off
stopf = function(fmt, ..., domain = NULL) {
  stop(gettextf(fmt, ..., domain = NULL), domain = NA, call. = FALSE)
}

# uncovered for now, used in msgmerge.R
warningf = function(fmt, ..., immediate. = FALSE, noBreaks. = FALSE) {
  warning(gettextf(fmt, ..., domain = NULL), domain = NA, call. = FALSE, immediate. = immediate., noBreaks. = noBreaks.)
}

messagef = function(fmt, ..., appendLF = TRUE) {
  message(gettextf(fmt, ..., domain = NULL), domain = NA, appendLF = appendLF)
}

# not actually used yet in package src, so commenting out for test coverage, but leaving in for illustration
# packageStartupMessagef = function(fmt, ..., appendLF = TRUE) {
#   packageStartupMessage(gettextf(fmt, ..., domain = NULL), domain = NA, appendLF = appendLF)
# }
