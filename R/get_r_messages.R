# Extended/adapted version of tools::xgettext. Mainly:
#   (1) We want the results of both asCall=TRUE and asCall=FALSE together
#   (2) We want to keep the caller (e.g. stop(), message(), etc.) as well
# To pair the results, instead of the asCall=TRUE approach, use deparse()
#   of any messaging input (to be shown to user later as an "in-context"
#   version of the strings to translate). Things can be a little wonky,
#   however, e.g. for how asCall=TRUE/FALSE handles domain=NA and
#   nested strings
get_r_messages <- function (dir, verbose = FALSE) {
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
          } else if (as.character(e[[1L]]) == "gettext" && !is.null(names(e))) {
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
      call_i <<- call_i + 1L
      out[[f]][[call_i]] <<- character()
      if (!suppress) names(out[[f]])[call_i] <<- deparse1(e)
      if (as.character(e[[1L]]) == "gettextf") {
        e <- match.call(gettextf, e)
        e <- e["fmt"]
      }
      literal_strings <- character()
      for (i in seq_along(e)) find_string_literals(e[[i]], suppress)
      out[[f]][[call_i]] <<- trimws(literal_strings)
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) find_strings(e[[i]])
    }
  }

  for (f in r_files) {
    if (verbose) message(gettextf("parsing '%s'", f, domain="R-tools"), domain = NA)
    call_i = 0L
    for (e in parse(file = f)) find_strings(e)
    # drop calls without literal strings, e.g. stop(msg) (i.e. not stop("msg"))
    out[[f]] = out[[f]][lengths(out[[f]]) > 0L]
  }
  out[lengths(out) > 0L]
}


# these functions all have a domain= argument. taken from the xgettext source, but could be
#   refreshed with the following (skipping bindtextdomain):
# for (obj in ls(BASE <- asNamespace('base'))) {
#     if (!is.function(f <- get(obj, envir = BASE))) next
#     if (is.null(f_args <- args(f))) next
#     if (any(names(formals(f_args)) == 'domain')) cat(obj, '\n')
# }
MSG_FUNS <- c("warning", "stop", "message", "packageStartupMessage", "gettext", "gettextf")
