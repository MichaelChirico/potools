#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

void b(SEXP x);
void c(SEXP x);

void b(SEXP x) {
  error(_("I really wish you'd do something else"));
}

void c(SEXP x) {
  Rprintf(_("Here's what is wrong: %s %s\n"), "a", "b");
}
