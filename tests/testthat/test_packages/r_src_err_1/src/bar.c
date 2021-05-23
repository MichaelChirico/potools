#include <R.h>
#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
#else
#define _(String) (String)
#endif

// intentionally terminate the file before the char array ends for coverage
static void glam(SEXP x) {
  Rprintf("an untranslated string\n
