#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("data.table", String)
// no-op macro for marking strings for translation at compile-time
//   without actually translating them until run-time
#define N_(String) String
#else
#define _(String) (String)
#endif
