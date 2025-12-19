catf = function(fmt, ..., sep=" ", domain="R-potools") {
  cat(gettextf(fmt, ..., domain=domain), sep=sep)
}

# tends to make some really repetitive messages with call.=TRUE, so always turn it off
stopf = function(fmt, ..., domain="R-potools") {
  stop(gettextf(fmt, ..., domain=domain), domain = NA, call. = FALSE)
}

# uncovered for now, used in msgmerge.R
warningf = function(fmt, ..., immediate.=FALSE, noBreaks.=FALSE, domain="R-potools") {
  warning(gettextf(fmt, ..., domain=domain), domain = NA, call. = FALSE, immediate. = immediate., noBreaks. = noBreaks.)
}

messagef = function(fmt, ..., appendLF=TRUE, domain="R-potools") {
  message(gettextf(fmt, ..., domain=domain), domain = NA, appendLF = appendLF)
}

# not actually used yet in package src, so commenting out for test coverage, but leaving in for illustration
# packageStartupMessagef = function(fmt, ..., appendLF = TRUE) {
#   packageStartupMessage(gettextf(fmt, ..., domain = domain), domain = NA, appendLF = appendLF)
# }
