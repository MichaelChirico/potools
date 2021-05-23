#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext("rSrcMsg", String)
#else
#define _(String) (String)
#endif
