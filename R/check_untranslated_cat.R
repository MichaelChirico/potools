# check a package for calls to cat() that have untranslated strings (i.e.,
#   aren't passed through gettext or ngettext)
check_untranslated_cat <- function (message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)
  # not iron-clad but it's a good first pass
  cat_calls = unique(
    message_data[message_source == "R" & type == "singular" & grepl("cat(", call, fixed = TRUE)],
    by = 'call'
  )
  if (!nrow(cat_calls)) return(diagnostic_schema())

  cat_calls[ , 'call_expr' := lapply(call, str2lang)]
  cat_calls = cat_calls[
    vapply(call_expr, function(x) x[[1L]] %is_base_call% 'cat', logical(1L))
  ]
  if (!nrow(cat_calls)) return(diagnostic_schema())

  cat_calls = cat_calls[vapply(call_expr, no_translated_subcall, logical(1L))]
  if (!nrow(cat_calls)) return(diagnostic_schema())

  # some of these will return "" which we can filter
  for (ii in seq_len(nrow(cat_calls))) {
    set(cat_calls, ii, "replacement", build_suggested_cat_call(cat_calls$call_expr[[ii]]))
  }

  return(cat_calls[
    nzchar(replacement),
    .(call, file, line_number, replacement)
  ])
}
attr(check_untranslated_cat, "diagnostic_tag") = "untranslated messaging calls passed through cat()"

build_suggested_cat_call = function(e) {
  if (is.null(names(e))) {
    named_idx = rep(FALSE, length(e))
    sep = " "
    named_arg_str = ""
  } else {
    if ("file" %in% names(e)) return("")
    # for non-file output, this is ignored
    if (length(append_idx <- which(names(e) == "append"))) {
      e = e[-append_idx]
    }
    if (length(sep_idx <- which(names(e) == "sep"))) {
      # maybe could signal that this is being skipped somehow?
      if (!is.character(e[[sep_idx]])) return("")
      sep = as.character(e[[sep_idx]])
      e = e[-sep_idx]
    } else {
      sep = " "
    }
    if (is.null(names(e))) {
      named_idx = rep(FALSE, length(e))
      named_arg_str = ""
    } else {
      named_idx = names(e) %chin% c("fill", "labels")
      named_arg_str = sprintf(
        ", %s",
        toString(sprintf("%s=%s", names(e)[named_idx], as.character(e[named_idx])))
      )
    }
  }
  encodeString(sprintf(
    'cat(%s%s)',
    gettextify(e[!named_idx][-1L], sep),
    named_arg_str
  ))
}

# check if an expression has any translated sub-call, e.g.
#   cat(gettext("Done."), "\n"). See #66 for more details.
no_translated_subcall <- function(e) {
  if (!is.call(e)) return(TRUE)
  if (is.name(e[[1L]]) && e[[1L]] %chin% c("gettext", "gettextf", "ngettext")) return(FALSE)
  return(all(vapply(e[-1L], no_translated_subcall, logical(1L))))
}
