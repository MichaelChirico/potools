#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

void hello_world(SEXP x) {
  // odd-ish spacing in implicit-cat char arrays, and implicit cat ending with macro
  Rprintf(_("that's a mighty big %"  PRId64"-sized wall over %"PRIu64), 100LL, 10L);
  // char array containing a comment that shouldn't be treated as such
  Rprintf(_("/* this is what a C comment looks like */"));
  Rprintf(_("// this is what a C comment looks like"));
  return;
}
