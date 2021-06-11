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
  Rprintf("an untranslated string\n");
  // test platform-robust format specification as done here
  Rprintf(_("an translated templated string: %"PRId64"\n"), 10000LL);
  error("an untranslated error");
}

char *stardust(SEXP z) {
  char *msg = "hijklmno";
  // not clear whether this should be marked as needing translation...
  Rprintf(msg);
  return "abcdefg";
}

/* Add a call here to make sure comment skipping works: Rprintf(_("hi")); */
static void ziggy(SEXP y, SEXP z) {
  // nested parentheses before we reach the end of the call
  warning(_("a translated warning: %s\n"), stardust(z));
  return;
}
