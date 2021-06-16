#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

void goodbye_sun(SEXP x) {
  // just testing that MSG.c comes before msg.c
  //   and that the src file marker gets the line number right when call & array are on different lines.
  //   use this file since we'll need to test the exact line # & expect this file to be pretty stable.
  Rprintf(_(
      "any new message"));
  return;
}
