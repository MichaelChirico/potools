#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

void hello_world(SEXP x) {
  // odd-ish spacing in implicit-cat char arrays, and implicit cat ending with macro
  Rprintf(_("that's a mighty big %"  PRId64"-sized wall over %"PRIu64), 100LL, 10L);

  // char array containing a comment that shouldn't be treated as such; also trailing whitespace
  Rprintf(_("/* this is what a C comment looks like */ "));
  // also testing trailing whitespace action
  Rprintf(_("// this is what a C comment looks like %s "), "abc");

  // a very long message to test string wrapping
  // TODO(#78): more thorough testing of this behavior
  Rprintf(_(
    "01234567890123456789.01234567890123456789"
    "01234567890123456789.01234567890123456789"
    "01234567890123456789.01234567890123456789"
    "01234567890123456789.01234567890123456789"
  ));

  // An array as a variable is passed, see #83
  char * msg = "an error occurred";
  error(_(msg));

  // Not thrown off by a char literal ", (, or ) (right-paren comes first to keep imbalance)
  *msg = '"';
  *msg = ')';
  *msg = '(';
  // also make sure the char array jump is exactly right (if we over-jump here we won't find the right-parent)
  foo(msg, '.');
  // get the jump right in the case of an escaped char (#135)
  foo(msg, '\0');

  // repeat a bunch of times to test the strwrap behavior of file markers
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);

  // xgettext prefers splitting lines at templates, but bumps the ' to the next line as well.
  error(_("error in evaluating the argument '%s' in selecting a method for function '%s': %s"), "abc", "def", "ghi");

  // xgettext always splits at a newline, regardless of width
  error(_("This message\nSpans two lines"));
  error(_("This one does not\n"));

  // base has a few of these strewn about -- ideally they would be replaced by PRId64 & family
  error(_("Exotic formatters like %I32u, %llx, %li"));
  return;
}
