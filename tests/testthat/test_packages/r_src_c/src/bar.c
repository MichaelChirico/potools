#include <R.h>
#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#else
#define _(String) (String)
#endif

// include this comment here to test comment skipping works
static void glam(SEXP x) {
  Rprintf("an untranslated string\n");
  Rprintf(_("an translated templated string: %s\n"), "foo");
  error("an untranslated error");
}

/* Add a call here to make sure comment skipping works: Rprintf(_("hi")); */
static void ziggy(SEXP y, SEXP z) {
  warning(_("a translated warning: %s\n"));
  return;
}
