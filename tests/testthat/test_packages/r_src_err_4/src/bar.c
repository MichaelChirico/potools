#include <R.h>
#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#else
#define _(String) (String)
#endif

static void glam(SEXP x) {
  // array concatenation with an illegal intermediate character
  Rprintf(_("an translated " 0 "templated string: %s\n")
