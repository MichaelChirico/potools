#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

static void glam(SEXP x);
static void ziggy(SEXP y, SEXP z);
char *stardust(SEXP z);

// include this comment here to test comment skipping works
static void glam(SEXP x) {
  // test platform-robust format specification as done here; add spacing on either side for #47
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
}

char *stardust(SEXP z) {
  // test that '//' inside a char array isn't mistaken for a comment by preprocess()
  return "abcdefg and a URL: http://github.com";
}

/* Add a call here to make sure comment skipping works: Rprintf(_("hi")); */
static void ziggy(SEXP y, SEXP z) {
  // nested parentheses before we reach the end of the call; use the line-continuation \ for #47
  warning(_("a translated "\
"warning: %s\n"), stardust(z));
  return;
}
