stopf = function(fmt, ..., call. = TRUE, domain = NULL) {
  stop(gettextf(fmt, ..., domain = NULL), domain = NA, call. = call.)
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
