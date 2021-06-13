/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2018  The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef Win32
//#define HAVE_PANGOCAIRO 1
#define HAVE_CAIRO_SVG 1
#define HAVE_CAIRO_PDF 1
#define HAVE_CAIRO_PS 1

# define raise our_raise
# include <Defn.h>
# undef raise
#else
# include <Defn.h>
#endif

#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include <Defn.h>
#include "Fileio.h"		/* R_fopen */

#include "cairoBM.h"

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext ("grDevices", String)
#else
#define _(String) (String)
#endif

#define NO_X11 1
#include "cairoFns.c"

static Rboolean
BM_Open(pDevDesc dd, pX11Desc xd, int width, int height)
{
    char buf[PATH_MAX];
    cairo_status_t res;
    if (xd->type == PNG || xd->type == JPEG ||
	xd->type == TIFF || xd->type == BMP ||
        xd->type == PNGdirect) {
	xd->cs = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
					    xd->windowWidth,
					    xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        cairo_set_operator(xd->cc, CAIRO_OPERATOR_OVER);
        cairo_reset_clip(xd->cc);
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#ifdef HAVE_CAIRO_SVG
    else if(xd->type == SVG) {
        snprintf(buf, PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_svg_surface_create(buf,
                                          (double)xd->windowWidth,
                                          (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            xd->cs = NULL;
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        if(xd->onefile)
            cairo_svg_surface_restrict_to_version(xd->cs, CAIRO_SVG_VERSION_1_2);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
#ifdef HAVE_CAIRO_PDF
    else if(xd->type == PDF) {
        snprintf(buf, PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_pdf_surface_create(buf,
                                          (double)xd->windowWidth,
                                          (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                              xd->fallback_dpi);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
#ifdef HAVE_CAIRO_PS
    else if(xd->type == PS) {
        snprintf(buf, PATH_MAX, R_CAIRO_FN(xd), xd->npages + 1);
        xd->cs = cairo_ps_surface_create(buf,
                                         (double)xd->windowWidth,
                                         (double)xd->windowHeight);
        res = cairo_surface_status(xd->cs);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
// We already require >= 1.2
#if CAIRO_VERSION_MAJOR > 2 || CAIRO_VERSION_MINOR >= 6
        if(!xd->onefile)
            cairo_ps_surface_set_eps(xd->cs, TRUE);
#endif
        cairo_surface_set_fallback_resolution(xd->cs, xd->fallback_dpi,
                                              xd->fallback_dpi);
        xd->cc = cairo_create(xd->cs);
        res = cairo_status(xd->cc);
        if (res != CAIRO_STATUS_SUCCESS) {
            warning("cairo error '%s'", cairo_status_to_string(res));
            return FALSE;
        }
        cairo_set_antialias(xd->cc, xd->antialias);
    }
#endif
    else
	error(_("unimplemented cairo-based device"));

    CairoInitPatterns(xd);
    CairoInitClipPaths(xd);
    CairoInitMasks(xd);
    xd->appending = 0;

    return TRUE;
}
