#include <R.h>
#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#else
#define _(String) (String)
#endif

static void glam(SEXP x) {
  // intentionally end the file before completing the translation macro call
  Rprintf(_("an translated templated string: %s\n")
