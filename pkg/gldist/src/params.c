/*  Copyright (C) 2012 Yohan Chalabi
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 or 3
 *  of the License (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <Rinternals.h>
#include "gldist.h"

void
gldist_params(double *params, SEXP arg1, SEXP med, SEXP iqr, SEXP chi, SEXP xi) {

    int med_len = length(med);

    if (!isNumeric(arg1) || !isNumeric(med) || !isNumeric(iqr) ||
	!isNumeric(chi) || !isNumeric(xi))
	Rf_error("Non-numeric argument");

    if (med_len == 1) {

	if (length(iqr) > 1) Rf_error("iqr must be of length 1");
	if (length(chi) > 1) Rf_error("chi must be of length 1");
	if (length(xi) > 1) Rf_error("xi must be of length 1");

	params[0] = asReal(med);
	params[1] = asReal(iqr);
	params[2] = asReal(chi);
	params[3] = asReal(xi);

    } else if (med_len == 4) {
	/* case when parameters are passed as a vector of length 4 */

	params[0] = REAL(med)[0];
	params[1] = REAL(med)[1];
	params[2] = REAL(med)[2];
	params[3] = REAL(med)[3];

    } else {

	Rf_error("med must be of length 1 or 4");
    }

}
