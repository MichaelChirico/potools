# take a call like stop("a", i, "b", j) and suggest
#   stop(domain=NA, gettextf("a%sb%s", i, j, domain="R-pkg"))
build_gettextf_call = function(txt, package) {
  if (!length(txt)) return(invisible())
  e = str2lang(txt)

  string = character(length(e) - 1L)
  str_idx = which(vapply(e[-1L], is.character, logical(1L)))
  string[str_idx] = as.character(e[str_idx + 1L])
  string[-str_idx] = '%s'

  lang_idx = which(vapply(e[-1L], is.language, logical(1L)))

  sprintf(
    '%s(domain=NA, gettextf("%s", %s, domain="R-%s"))',
    as.character(e[[1L]]),
    paste(string, collapse=''),
    toString(vapply(lang_idx, function(ii) deparse1(e[[ii + 1L]]), character(1L))),
    package
  )
}
