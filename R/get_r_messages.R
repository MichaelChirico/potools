# Extended/adapted/combined version of tools::{x,xn}gettext. Mainly:
#   (1) We want the results of both asCall=TRUE and asCall=FALSE together
#   (2) We want to keep the caller (e.g. stop(), message(), etc.) as well
# To pair the results, instead of the asCall=TRUE approach, use deparse()
#   of any messaging input (to be shown to user later as an "in-context"
#   version of the strings to translate). Things can be a little wonky,
#   however, e.g. for how asCall=TRUE/FALSE handles domain=NA and
#   nested strings
get_r_messages <- function (dir, verbose = FALSE) {
  dir = file.path(dir, 'R')
  r_files = list_r_files(dir)
  for (os in c("unix", "windows")) {
    os_dir = file.path(dir, os)
    if (dir.exists(os_dir)) r_files = c(r_files, list_r_files(os_dir))
  }
  # somehow on windows I was seeing absolute paths with \ but paths
  #   from list.files as / -- normalizePath makes it consistent
  r_files = normalizePath(r_files)
  singular = plural = vector("list", length = length(r_files))
  names(singular) = names(plural) = r_files

  # inherits singular_i, s_data
  find_singular_strings = function(e) {
    # inherits literal_strings
    find_string_literals = function(e, suppress) {
      if (is.character(e)) {
        if(!suppress) literal_strings <<- c(literal_strings, e)
      } else if (is.call(e)) {
        # both gettext & gettextf only have one possible string to extract, so separate
        if (e[[1L]] %is_base_call% c("gettext", "gettextf")) {
          suppress <- do_suppress(e)
          if (e[[1L]] %is_base_call% "gettextf") {
            e <- match.call(gettextf, e)
            e <- e["fmt"] # just look at fmt arg
          } else if (e[[1L]] %is_base_call% "gettext" && !is.null(names(e))) {
            e <- e[names(e) != "domain"] # remove domain arg
          }
        } else if (identical(e[[1L]], quote(ngettext)) || identical(e[[1L]], quote(base::ngettext))) return()
        for(i in seq_along(e)) find_string_literals(e[[i]], suppress)
      }
    }
    if (is.call(e) && e[[1L]] %is_base_call% MSG_FUNS) {
      suppress = do_suppress(e)
      if (!is.null(names(e))) {
        e <- e[!names(e) %in% c("call.", "immediate.", "domain")]
      }
      # keep call name (e[[1L]]); fine to ignore in find_strings2
      singular_i <<- singular_i + 1L
      s_data[[singular_i]] <<- character()
      if (!suppress) names(s_data)[singular_i] <<- deparse1(e)
      if (e[[1L]] %is_base_call% "gettextf") {
        e = match.call(gettextf, e)
        e = e["fmt"]
      }
      literal_strings <- character()
      for (i in seq_along(e)) find_string_literals(e[[i]], suppress)
      s_data[[singular_i]] <<- trimws(literal_strings)
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) find_singular_strings(e[[i]])
    }
  }

  # inherits plural_i, p_data
  find_plural_strings = function(e) {
    if (is.call(e) && e[[1L]] %is_base_call% "ngettext") {
      e = match.call(ngettext, e)
      suppress = do_suppress(e)
      if (!suppress && is.character(e[["msg1"]]) && is.character(e[["msg2"]])) {
        plural_i <<- plural_i + 1L
        p_data[[plural_i]] <<- list(e[["msg1"]], e[["msg2"]])
        names(p_data)[plural_i] <<- deparse1(e)
      }
    }
    else if (is.recursive(e))
      for (i in seq_along(e)) find_plural_strings(e[[i]])
  }

  for (f in r_files) {
    if (verbose) message(gettextf("parsing '%s'", f, domain="R-tools"), domain = NA)
    singular_i = plural_i = 0L
    s_data <- p_data <- vector("list")
    for (e in parse(file = f, keep.source = TRUE)) {
      find_singular_strings(e)
      find_plural_strings(e)
    }

    singular[[f]] = unnest_call(s_data, plural=FALSE)
    plural[[f]] = unnest_call(p_data, plural=TRUE)
  }
  msg = rbind(
    singular = rbindlist(singular, idcol='file'),
    plural = rbindlist(plural, idcol='file'),
    idcol = 'type', fill = TRUE, use.names = TRUE
  )
  if (nrow(msg) == 0L) {
    return(data.table(
      type = character(),
      file = character(),
      call = character(),
      msgid = character(),
      plural_msgid = list(),
      is_repeat = logical()
    ))
  }
  msg[type == 'singular', msgid := escape_string(msgid)]
  msg[type == 'plural', plural_msgid := lapply(plural_msgid, escape_string)]
  msg[ , 'is_repeat' := type == 'singular' & duplicated(msgid)]
  msg[]
}

# these functions all have a domain= argument. taken from the xgettext source, but could be
#   refreshed with the following (skipping bindtextdomain and .makeMessage):
# for (obj in ls(BASE <- asNamespace('base'))) {
#     if (!is.function(f <- get(obj, envir = BASE))) next
#     if (is.null(f_args <- args(f))) next
#     if (any(names(formals(f_args)) == 'domain')) cat(obj, '\n')
# }
MSG_FUNS = c("warning", "stop", "message", "packageStartupMessage", "gettext", "gettextf")

# be sure to apply encodeString, which converts "\n" to \\n as required when
#   (potentially) writing this out to .po later
unnest_call = function(data, plural) {
  empty = !any(lengths(data))
  if (empty) return(data.table(NULL))
  calls = names(data)
  names(data) = NULL
  if (plural) return(data.table(call = calls, msgid = "", plural_msgid = data))
  data.table(
    call = rep(calls, lengths(data)),
    msgid = unlist(data),
    plural_msgid = list()
  )
}
