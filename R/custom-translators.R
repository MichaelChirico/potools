stopf = function(fmt, ..., domain = NULL) {
  stop(domain = NA, gettextf(fmt, ..., domain = NULL))
}

warningf = function(fmt, ...) {
  warning(domain = NA, gettextf(fmt, ..., domain = NULL))
}

messagef = function(fmt, ...) {
  message(domain = NA, gettextf(fmt, ..., domain = NULL))
}

packageStartupMessage(fmt, ...) {
  packageStartupMessage(gettextf(fmt, ..., domain = NULL))
}
