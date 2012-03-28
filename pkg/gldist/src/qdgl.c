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

double
gldist_do_dS1(double x, void *info) {
    return 1. / x + 1. / (1. - x);
}

double
gldist_do_dS2(double x, void *info) {
    double alpha = ((double*) info)[0];
    return 1. / x + pow(1. - x, alpha - 1.);
}

double
gldist_do_dS3(double x, void *info) {
    double beta = ((double*) info)[0];
    return pow(x, beta - 1.) + 1. / (1. - x);
}

double
gldist_do_dS4(double x, void *info) {
    double pv[2], ev[2];
    double Sv[2];
    int j;
    ev[0] = ((double*) info)[0];
    ev[1] = ((double*) info)[1];
    pv[0] = x; pv[1] = 1. - x;
    for (j = 0; j < 2; ++j)
        Sv[j] = pow(pv[j], ev[j] - 1.);
    return Sv[0] + Sv[1];
}

/* Macros to beautify code in do_qdgl() */
#define DO_LOOP					\
    for (i = 0; i < n; ++i) {			\
	px = p[i];				\
	if (ISNAN(px)) {			\
	    qx = px;				\
	} else if (px < 0. || px > 1.) {	\
	    qx = R_NaN;				\
	    warn = 1;				\
	} else {				\
	    dS = DSFUN;				\
	    qx = a * dS;			\
	}					\
	qd[i] = qx;				\
    }						\
    if (warn) Rf_warning("NaNs produced");	\

void
gldist_do_qdgl(double *qd, double * const p, double med, double iqr, double chi, double xi, int n)
{

    /* Note try to limit as much as possible logical test in the for loop */
    /* more over we use seperate variables to help compilers to
       auto-vectorize the calls to mathematical functions. */

    double qx, px;
    double alpha, beta;
    double ev[2];
    double Sqv[2], qv[2] = {.25, .75};
    double a, b, c;
    double dS;
    int warn = 0, flag = 0;
    int i;

    WHICH_CASE(flag, med, iqr, chi, xi);

    switch (flag) {

    case 1:
	/* (ISNA(med) || ISNA(iqr) || ISNA(chi) || ISNA(xi)) */
	for (i = 0; i < n; ++i)
	    qd[i] = NA_REAL;
	break;

    case 2:
	/* (ISNAN(med) || ISNAN(iqr) || ISNAN(chi) || ISNAN(xi)) */
	for (i = 0; i < n; ++i)
	    qd[i] = R_NaN;
	break;

    case 3:
	/* (chi == -1. && xi == 0.) */
	a = iqr / log(3.);
#define DSFUN (1. / px)
	DO_LOOP
#undef  DSFUN
	break;

    case 4:
	/* (chi == 1. && xi == 0.) */
	a = iqr / log(3.);
#define DSFUN (1. / (1. - px))
	DO_LOOP
#undef  DSFUN
	break;

    case 5:
	/* (iqr <= 0. || chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) */
	for (i = 0; i < n; ++i)
	    qd[i] = R_NaN;
	Rf_warning("NaNs produced");
	break;

    case 6:
	/* (chi == 0 && xi == .5) */
	for (i = 0; i < 2; i++)
	    Sqv[i] = gldist_do_S1(qv[i], (void *) NULL);
	a = iqr / (Sqv[1] - Sqv[0]);
#define DSFUN gldist_do_dS1(px, (void *) NULL)
	DO_LOOP
#undef  DSFUN
	break;

    case 7:
	/* (xi != 0 && xi == .5 * (1. + chi)) */
	alpha = (.5 - xi) / sqrt(xi * (1. - xi));
	for (i = 0; i < 2; i++)
	    Sqv[i] = gldist_do_S2(qv[i], &alpha);
	a = iqr / (Sqv[1] - Sqv[0]);
#define DSFUN gldist_do_dS2(px, &alpha)
	DO_LOOP
#undef  DSFUN
	break;

    case 8:
	/* (xi != 0 && xi == .5 * (1. - chi)) */
	beta = chi / sqrt(1. - chi * chi);
	for (i = 0; i < 2; i++)
	    Sqv[i] = gldist_do_S3(qv[i], &beta);
	a = iqr / (Sqv[1] - Sqv[0]);
#define DSFUN gldist_do_dS3(px, &beta)
	DO_LOOP
#undef  DSFUN
	break;

    case 9:
	/* (chi == 0 && (xi == XIUNIF1 || xi == XIUNIF2)) */
	a = med - iqr;
	b = med + iqr;
	c = b - a;
	for (i = 0; i < n; ++i) {
	    px = p[i];
	    if (ISNAN(px)) {
		qx = px;
	    } else if (px < 0 || px > 1.) {
		qx = R_NaN;
		warn = 1;
	    } else {
		qx = c;
	    }
	    qd[i] = qx;
	}
	if (warn) Rf_warning("NaNs produced");
	break;

    default:
	alpha = .5 * (.5 - xi) / sqrt(xi * (1. - xi));
	beta = .5 * chi / sqrt(1. - chi * chi);
	ev[0] = alpha + beta;
	ev[1] = alpha - beta;
	for (i = 0; i < 2; i++)
	    Sqv[i] = gldist_do_S4(qv[i], ev);
	a = iqr / (Sqv[1] - Sqv[0]);
#define DSFUN gldist_do_dS4(px, ev)
	DO_LOOP
#undef  DSFUN
	break;

    }
}

#undef DO_LOOP

SEXP
gldist_qdgl(SEXP p, SEXP med, SEXP iqr, SEXP chi, SEXP xi) {

    double params[4];
    int len;

    SEXP q;

    /* build parameters vector */
    gldist_params(params, p, med, iqr, chi, xi);
    /* explicitly convert to a real vector to catch non-numeric NAs */
    p = coerceVector(p, REALSXP);

    /* create returned object */
    len = length(p);
    PROTECT(q = allocVector(REALSXP, len));

    /* Compute quantiles */
    gldist_do_qdgl(REAL(q), REAL(p), params[0], params[1], params[2], params[3] , len);

    UNPROTECT(1);
    return q;

}
