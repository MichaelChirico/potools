# check a package for calls to cat() that have untranslated strings (i.e.,
#   aren't passed through gettext or ngettext)
check_untranslated_cat <- function (exprs, package) {
  # inherits call_i, s_data
  find_untranslated_strings = function(e) {
    if (is.call(e) && e[[1L]] %is_base_call% 'cat') {
      if (is.null(names(e))) {
        named_idx = rep(FALSE, length(e))
        named_arg_str = ""
      } else {
        if ("file" %chin% names(e)) return(NULL)
        named_idx = names(e) %chin% c("sep", "fill", "labels", "append")
        # NB: also works when !length(named_idx)
        named_arg_str = toString(sprintf("%s=%s", names(e)[named_idx], as.character(e[named_idx])))
      }
      if (any(vapply(e[!named_idx], is.character, logical(1L)))) {
        call_i <<- call_i + 1L
        f_data[[call_i]] <<- list(
          call_text = deparse1(e),
          suggested = build_gettextf_call(e[!named_idx], package, named_arg_str)
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

    cat_calls[[f]] = f_data
  }


}
