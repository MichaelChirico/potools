#include <stdio.h>
#include <inttypes.h>

#include <R.h>
#include <Rinternals.h>
#include "po.h"

// a message not found within a call still gets assigned a source line, #133
#define MSG _("a message in a macro %s")

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

  // duplicate of the array in the weirdly long filename, for testing the corner case of file marker wrapping
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);
  Rprintf(_("an translated templated string: %"  PRId64  "\n"), 10000LL);

  // xgettext always splits at a newline, regardless of width
  error(_("This message\nSpans two lines"));
  error(_("This one does not\n"));

  // base has a few of these strewn about -- ideally they would be replaced by PRId64 & family
  error(_("Exotic formatters like %I32u, %llx, %li, %ls, %lc"));

  // wrapping behavior for \", see #91
  // don't break for these (preceded by a digit, `(`, `)`, or `'`)
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456\"890"));
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345(\"890"));
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345'\"890"));
  // do break for these (preceded by other ASCII characters)
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345a\"890"));
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345A\"890"));
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345#\"890"));
  error(_("0123456789012345678901234567890123456789012345678901234567890123456789012345@\"890"));

  // wrapping behavior for templates, see #90, #150
  // TODO(#78): fix this -- currently causes an error
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 .%s."));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ?%s?"));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 ;%s;"));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 /%s/"));

  // do break for these
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 '%s'"));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 [%s]"));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 |%s|"));
  error(_("01234567890123456789012345678901234567890123456789012345678901234567890123456 -%s-"));

  // ternary operator inside _(), #154
  bool test=true;
  error(_(test ? "abc" : "def"));
  // also testing unrecognized macro usage
  error(_(xxx "abc" "def"));
  error(_("abc" xxx "def" yyy));
  error(_("abc" "def" xxx));

  // dgettext messages are included, & regardless of domain, #153
  error(dgettext("fakeDomain", "abcdef"));

  // snprintf calls always get c-format tagged, even when the other appearance of the msgid is not a template call
  snprintf(BUF, 100, _("This one does not\n"));

  // don't fail when using the macro message
  error(MSG, "unknown");
  return;
}
