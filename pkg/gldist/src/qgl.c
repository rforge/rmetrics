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
gldist_do_S1(double x, void *info) {
    return log(x) - log(1. - x);
}

double
gldist_do_S2(double x, void *info) {
    double alpha = ((double*) info)[0];
    return log(x) - (pow(1. - x, alpha) - 1.) / alpha;
}

double
gldist_do_S3(double x, void *info) {
    double beta = ((double*) info)[0];
    return (pow(x, beta) - 1.) / beta - log(1. - x);
}

double
gldist_do_S4(double x, void *info) {

    double pv[2], ev[2];
    double S, Sv[2];
    int j;

    ev[0] = ((double*) info)[0];
    ev[1] = ((double*) info)[1];

    pv[0] = x; pv[1] = 1. - x;
    for (j = 0; j < 2; ++j)
        Sv[j] = (pow(pv[j], ev[j]) - 1.) / ev[j];
    S = Sv[0] - Sv[1];

    return S;
}

/* Macros to beautify code in do_qgl() */
#define DO_LOOP					\
    for (i = 0; i < n; ++i) {			\
	px = p[i];				\
	if (ISNAN(px)) {			\
	    qx = px;				\
	} else if (px < 0. || px > 1.) {	\
	    qx = R_NaN;				\
	    warn = 1;				\
	} else {				\
	    S = SFUN;				\
	    qx = a + b * S;			\
	}					\
	q[i] = qx;				\
    }						\
    if (warn) Rf_warning("NaNs produced");	\

void
gldist_do_qgl(double *q, double * const p, double med,
	      double iqr, double chi, double xi, int n) {

    /* Note try to limit as much as possible logical test in the for loop */
    /* more over we use seperate variables to help compilers to auto-vectorize the calls to mathematical functions. */

    double qx, px;
    double alpha, beta;
    double Sv[2], pv[2], ev[2];
    double Sqv[3], qv[3] = {.25, .5, .75};
    double a, b, c;
    double S;
    int warn = 0, flag = 0;
    int i;

    WHICH_CASE(flag, med, iqr, chi, xi);

    switch (flag) {

    case 1:
	/* (ISNA(med) || ISNA(iqr) || ISNA(chi) || ISNA(xi)) */
	for (i = 0; i < n; ++i)
	    q[i] = NA_REAL;
	break;

    case 2:
	/* (ISNAN(med) || ISNAN(iqr) || ISNAN(chi) || ISNAN(xi)) */
	for (i = 0; i < n; ++i)
	    q[i] = R_NaN;
	break;

    case 3:
	/* (chi == -1. && xi == 0.) */
	c = log(3.);
	a = med + iqr * log(2.) / c;
	b = iqr / c;
#define SFUN log(px)
	DO_LOOP
#undef	SFUN
	break;

    case 4:
	/* (chi == 1. && xi == 0.) */
	c = log(3.);
	a = med - iqr * log(2.) / c;
	b = - iqr / c;
#define	SFUN log(1. - px)
	DO_LOOP
#undef  SFUN
	break;

    case 5:
	/* (iqr <= 0. || chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) */
	for (i = 0; i < n; ++i)
	    q[i] = R_NaN;
	Rf_warning("NaNs produced");
	break;

    case 6:
	/* (chi == 0 && xi == .5) */
	for (i = 0; i < 3; i++)
	    Sqv[i]  = gldist_do_S1(qv[i], (void *) NULL);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
#define SFUN gldist_do_S1(px, (void *) NULL)
	DO_LOOP
#undef  SFUN
	break;

    case 7:
	/* (xi != 0 && xi == .5 * (1. + chi)) */
	alpha = (.5 - xi) / sqrt(xi * (1. - xi));
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S2(qv[i], &alpha);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
#define	SFUN gldist_do_S2(px, &alpha)
	DO_LOOP
#undef	SFUN
	break;

    case 8:
	/* (xi != 0 && xi == .5 * (1. - chi)) */
	beta = chi / sqrt(1. - chi * chi);
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S3(qv[i], &beta);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
#define	SFUN gldist_do_S3(px, &beta)
	DO_LOOP
#undef  SFUN
	break;

    case 9:
	/* (chi == 0 && (xi == XIUNIF1 || xi == XIUNIF2)) */
	a = med - iqr;
	b = med + iqr;
	for (i = 0; i < n; ++i) {
	    px = p[i];
	    if (ISNAN(px)) {
		qx = px;
	    } else if (px < 0 || px > 1.) {
		qx = R_NaN;
		warn = 1;
	    } else {
		qx = a * (1. - px) + b * px;
	    }
	    q[i] = qx;
	}
	if (warn) Rf_warning("NaNs produced");
	break;

    default:
	alpha = .5 * (.5 - xi) / sqrt(xi * (1. - xi));
	beta  = .5 * chi / sqrt(1. - chi * chi);
	ev[0] = alpha + beta;
	ev[1] = alpha - beta;
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S4(qv[i], ev);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
#define SFUN gldist_do_S4(px, ev)
	DO_LOOP
#undef  SFUN
	break;

    }
}

#undef DO_LOOP

SEXP
gldist_qgl(SEXP p, SEXP med, SEXP iqr, SEXP chi, SEXP xi) {

    double params[4];
    int len;

    SEXP q;

    /* build parameter vector */
    gldist_params(params, p, med, iqr, chi, xi);
    /* explicitly convert to a real vector to catch non-numeric NAs */
    p = coerceVector(p, REALSXP);

    /* create returned object */
    len = length(p);
    PROTECT(q = allocVector(REALSXP, len));

    /* Compute quantiles */
    gldist_do_qgl(REAL(q), REAL(p), params[0], params[1], params[2], params[3], len);

    UNPROTECT(1);
    return q;

}
