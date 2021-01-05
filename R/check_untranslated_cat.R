# check a package for calls to cat() that have untranslated strings (i.e.,
#   aren't passed through gettext or ngettext)
check_untranslated_cat <- function (exprs, package) {
  # inherits call_i, s_data
  find_untranslated_strings = function(e) {
    if (is.call(e) && e[[1L]] %is_base_call% 'cat') {
      if (is.null(names(e))) {
        named_idx = rep(FALSE, length(e))
        sep = " "
        named_arg_str = ""
      } else {
        if ("file" %chin% names(e)) return(NULL)
        # for non-file output, this is ignored
        if (length(append_idx <- which(names(e) == "append"))) {
          e = e[-append_idx]
        }
        if (length(sep_idx <- which(names(e) == "sep"))) {
          # maybe could signal that this is being skipped somehow?
          if (!is.character(e[[sep_idx]])) return(NULL)
          sep = as.character(e[[sep_idx]])
          e = e[-sep_idx]
        } else {
          sep = " "
        }
        named_idx = names(e) %chin% c("fill", "labels")
        # NB: also works when !length(named_idx)
        named_arg_str = sprintf(
          ", %s",
          toString(sprintf("%s=%s", names(e)[named_idx], as.character(e[named_idx])))
        )
      }
      if (any(str_idx <- vapply(e[!named_idx], is.character, logical(1L)))) {
        call_i <<- call_i + 1L
        call_text = deparse1(e)
        e = e[!named_idx]
        suggested = sprintf(
          'cat(%s%s)',
          gettextify(e[-1L], sep, package),
          named_arg_str
        )

        f_data[[call_i]] <<- list(
          call_text = call_text,
          suggested = suggested
        )
      }
    } else if (is.recursive(e)) {
      for (i in seq_along(e)) find_untranslated_strings(e[[i]])
    }
  }

  cat_calls = vector("list", length(exprs))
  names(cat_calls) = names(exprs)

  for (ii in seq_along(exprs)) {
    call_i = 0L
    f_data <- vector("list")
    for (e in exprs[[ii]]) find_untranslated_strings(e)

    cat_calls[[ii]] = f_data
  }
  return(invisible())
}
