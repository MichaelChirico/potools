#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

void goodbye_sun(SEXP x) {
  // just testing that MSG.c comes before msg.c
  // also that \ as line continuation mid-array is handled properly, #91
  Rprintf(_("any old \
message"));
  return;
}
