# check a package for calls to cat() that have untranslated strings (i.e.,
#   aren't passed through gettext or ngettext)
check_untranslated_cat <- function (message_data) {
  if (!is.data.table(message_data)) message_data = as.data.table(message_data)
  # not iron-clad but it's a good first pass
  cat_calls = unique(
    message_data[message_source == "R" & type == "singular" & grepl("cat(", call, fixed = TRUE)],
    by = 'call'
  )
  if (!nrow(cat_calls)) return('n')

  cat_calls[ , 'call_expr' := lapply(call, str2lang)]
  cat_calls = cat_calls[
    vapply(call_expr, function(x) x[[1L]] %is_base_call% 'cat', logical(1L))
  ]
  if (!nrow(cat_calls)) return('n')

  cat_calls = cat_calls[vapply(call_expr, no_translated_subcall, logical(1L))]
  if (!nrow(cat_calls)) return('n')

  # some of these will return "" which we can filter
  for (ii in seq_len(nrow(cat_calls))) {
    set(cat_calls, ii, "suggested_call", build_suggested_cat_call(cat_calls$call_expr[[ii]]))
  }
  cat_calls = cat_calls[nzchar(suggested_call)]
  if (!nrow(cat_calls)) return('n')

  message(domain=NA, gettextf(
    'Found %d untranslated messaging calls passed through cat():',
    nrow(cat_calls)
  ))

  for (ii in seq_len(nrow(cat_calls))) {
    cat_calls[ii, cat(gettextf(
      '\n%s\n< File:%s, Line:%s >\nPotential replacement with gettextf():\n%s\n',
      call_color(call),
      file_color(file),
      file_color(line_number),
      build_gettextf_color(encodeString(suggested_call))
    ))]
  }
  return(prompt('Exit now to repair any of these? [y/N]'))
}

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
  sprintf(
    'cat(%s%s)',
    gettextify(e[!named_idx][-1L], sep),
    named_arg_str
  )
}

# check if an expression has any translated sub-call, e.g.
#   cat(gettext("Done."), "\n"). See #66 for more details.
no_translated_subcall <- function(e) {
  if (!is.call(e)) return(TRUE)
  if (is.name(e[[1L]]) && e[[1L]] %chin% c("gettext", "gettextf", "ngettext")) return(FALSE)
  return(all(vapply(e[-1L], no_translated_subcall, logical(1L))))
}
