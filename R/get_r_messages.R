# Extended/customized version of tools::xgettext. Mainly:
#   (1) We want the results of both asCall=TRUE and asCall=FALSE together
#   (2) We want to keep the caller (e.g. stop(), message(), etc.) as well
#   (3) Handle deduplication at a package level
get_r_messages <- function (dir, verbose = FALSE, asCall = TRUE) {
  dir = file.path(dir, 'R')
  r_files <- list_r_files(dir)
  for (os in c("unix", "windows")) {
    os_dir = file.path(dir, os)
    if (dir.exists(os_dir)) rfiles = c(r_files, list_r_files(os_dir))
  }
  out = vector("list", length = length(r_files))
  names(out) = r_files

  # inherits call_strings, literal_strings
  find_strings <- function(e) {
    # inherits literal_strings
    find_string_literals <- function(e, suppress) {
      if (is.character(e)) {
        if(!suppress) literal_strings <<- c(literal_strings, e)
      } else if (is.call(e)) {
        # both gettext & gettextf only have one possible string to extract, so separate
        if (is.name(e[[1L]]) && (as.character(e[[1L]]) %in% c("gettext", "gettextf"))) {
          suppress <- do_suppress(e)
          if (as.character(e[[1L]]) == "gettextf") {
            e <- match.call(gettextf, e)
            e <- e["fmt"] # just look at fmt arg
          } else if (as.character(e[[1L]]) == "gettext" &&
                     !is.null(names(e))) {
            e <- e[names(e) != "domain"] # remove domain arg
          }
        } else if (identical(e[[1L]], quote(ngettext))) return()
        for(i in seq_along(e)) find_string_literals(e[[i]], suppress)
      }
    }
    if (is_name_call(e) && as.character(e[[1L]]) %in% MSG_FUNS) {
      suppress <- do_suppress(e)
      if (!is.null(names(e))) {
        e <- e[!names(e) %in% c("call.", "immediate.", "domain")]
      }
      # keep call name (e[[1L]]); fine to ignore in find_strings2
      if (!suppress) call_strings <<- c(call_strings, as.character(e))
      if (as.character(e[[1L]]) == "gettextf") {
        e <- match.call(gettextf, e)
        e <- e["fmt"]
      }
      for (i in seq_along(e)) find_string_literals(e[[i]], suppress)
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) find_strings(e[[i]])
    }
  }

  for (f in r_files) {
    if (verbose) message(gettextf("parsing '%s'", f, domain="R-tools"), domain = NA)
    call_strings = literal_strings = character()
    for (e in parse(file = f)) find_strings(e)
    call_strings <- trimws(call_strings)
    literal_strings <- trimws(literal_strings)
    # NB: length(call_strings) > 0 is weaker than length(literal_strings) > 0
    if (length(call_strings)) out[[f]] <- list(call_strings, literal_strings)
  }
  out[lengths(out) > 0L]
}


# these functions all have a domain= argument
MSG_FUNS <- c("warning", "stop", "message", "packageStartupMessage", "gettext", "gettextf")
