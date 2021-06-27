#include "potoolsExample.h"
#include <R_ext/Rdynload.h>

SEXP reverse_int();

static const R_CallMethodDef callMethods[] = {{"reverse_int", (DL_FUNC) &reverse_int, 2}, {NULL, NULL, 0}};

void R_init_potoolsExample(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
