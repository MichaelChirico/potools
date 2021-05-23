# take a call like stop("a", i, "b", j) and suggest
#   stop(domain=NA, gettextf("a%sb%s", i, j))
build_gettextf_call = function(e) {
  if (!length(e)) return(invisible())
  e = str2lang(e)

  sprintf(
    '%s(domain=NA, %s)',
    as.character(e[[1L]]),
    gettextify(e[-1L])
  )
}
