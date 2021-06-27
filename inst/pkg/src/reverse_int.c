#include "potoolsExample.h"

void ReverseTemplateMessage(int n, char *s) {
  Rprintf(s, n);
}

SEXP reverse_int(SEXP x, SEXP verbose) {
  if (TYPEOF(x) != INTSXP) {
    error(_("reverse_int() only works on integer input, received %s"), type2char(TYPEOF(x)));
  }

  int n = LENGTH(x);
  SEXP out = PROTECT(allocVector(INTSXP, n));
  int *xp = INTEGER(x);
  int *outp = INTEGER(out);

  if (LOGICAL(verbose)[0]) {
    // a strange and poorly-designed messaging function for illustration
    ReverseTemplateMessage(n, "Reversing a vector with %d elements\n");
  }

  for (int i = 0; i < n; ++i) {
    outp[i] = xp[n - 1 - i];
  }

  UNPROTECT(1);
  return out;
}
