#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

static void glam(SEXP x);
static void ziggy(SEXP y, SEXP z);
char *stardust(SEXP z);

// N_ is used to prevent translation at compile time and defer it until build time, see #152 & R-ints
static char *msg[] = {N_("Don't translate me now.")};

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
  // just to demonstrate how an N_-marked message would be translated at run time
  warning(_(msg[0]));

  // c-format is applied to all snprintf calls, regardless of template markers like %s
  snprintf(BUF, 100, _("a simple message"));

  // test with/without formatting to make sure c-format is recorded correctly
  error(ngettext("singular", "plural", z));
  error(ngettext("singular %d", "plural %d", z));
  return;
}
