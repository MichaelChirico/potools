#include <R.h>
#include <Rinternals.h>

SEXP doNothing(SEXP x);

SEXP doNothing(SEXP x) {
  return R_NilValue;
}
