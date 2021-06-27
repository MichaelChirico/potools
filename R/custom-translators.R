# tends to make some really repetitive messages with call.=TRUE, so always turn it off
stopf = function(fmt, ..., domain = NULL) {
  stop(gettextf(fmt, ..., domain = NULL), domain = NA, call. = FALSE)
}

warningf = function(fmt, ..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE) {
  warning(gettextf(fmt, ..., domain = NULL), domain = NA, call. = call., immediate. = immediate., noBreaks. = noBreaks.)
}

messagef = function(fmt, ..., appendLF = TRUE) {
  message(gettextf(fmt, ..., domain = NULL), domain = NA, appendLF = appendLF)
}

packageStartupMessagef = function(fmt, ..., appendLF = TRUE) {
  packageStartupMessage(gettextf(fmt, ..., domain = NULL), domain = NA, appendLF = appendLF)
}
