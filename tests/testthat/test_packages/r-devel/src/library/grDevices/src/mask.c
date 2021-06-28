/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2014  The R Core Team
 *  Copyright (C) 2003	     The R Foundation
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/GraphicsEngine.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
// here as a regression test for #184
#define _(String) dgettext ("grDevices", String)
#else
#define _(String) (String)
#endif

SEXP setMask(SEXP args)
{
    pGEDevDesc dd = GEcurrentDevice();
    SEXP mask = CADR(args);
    SEXP ref = CADDR(args);
    ref = dd->dev->setMask(mask, ref, dd->dev);
    return ref;
}
