# take a call like stop("a", i, "b", j) and suggest
#   stop(domain=NA, gettextf("a%sb%s", i, j, domain="R-pkg"))
build_gettextf_call = function(e, package, args2S) {
  if (!length(e)) return(invisible())
  # as of now, only used for check_untranslated_cat
  if (is.language(e)) {
    args1 = ""
    # don't use gettextf for cat("a single message\n")
    if (length(e) == 2L) {
      return(sprintf('cat(gettext("%s", domain="R-%s"))', e[[2L]], package))
    }
  } else {
    e = str2lang(e)
    args1 = "domain=NA, "
    args2 = ""
  }

  string = character(length(e) - 1L)
  str_idx = which(vapply(e[-1L], is.character, logical(1L)))
  string[str_idx] = as.character(e[str_idx + 1L])
  string[-str_idx] = '%s'

  lang_idx = which(vapply(e[-1L], is.language, logical(1L)))

  sprintf(
    '%s(%sgettextf("%s", %s, domain="R-%s")%s)',
    as.character(e[[1L]]),
    args1,
    paste(string, collapse=''),
    toString(vapply(lang_idx, function(ii) deparse1(e[[ii + 1L]]), character(1L))),
    package,
    args2
  )
}
