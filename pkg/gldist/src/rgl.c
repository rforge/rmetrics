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

/* Macros to beautify code in do_qgl() */
#define DO_LOOP					\
    for (i = 0; i < n; ++i) {			\
	px = unif_rand();			\
	S = SFUN;				\
	x[i] = a + b * S;			\
    }

void
gldist_do_rgl(double *x, double med, double iqr, double chi, double xi, int n) {

    /* Note try to limit as much as possible logical test in the for loop */
    /* more over we use seperate variables to help compilers to
       auto-vectorize the calls to mathematical functions. */

    double px;
    double alpha, beta;
    double ev[2];
    double Sqv[3], qv[3] = {.25, .5, .75};
    double a, b, c;
    double S;
    int flag = 0;
    int i;

    /* First check the special cases when chi in {-1, 1} and xi in {0, 1} */
    WHICH_CASE(flag, med, iqr, chi, xi);

    /* take care of seed */
    GetRNGstate();

    switch (flag) {

    case 1:
	/* (ISNA(med) || ISNA(iqr) || ISNA(chi) || ISNA(xi)) */
	for (i = 0; i < n; ++i)
	    x[i] = NA_REAL;
	break;

    case 2:
	/* (ISNAN(med) || ISNAN(iqr) || ISNAN(chi) || ISNAN(xi)) */
	for (i = 0; i < n; ++i)
	    x[i] = R_NaN;
	break;

    case 3:
	/* (chi == -1. && xi == 0.) */
	a = iqr / log(3.);
	for (i = 0; i < n; ++i) {
	    px = unif_rand();
	    x[i] = med + a * log(2. * px);
	}
	break;

    case 4:
	/* (chi == 1. && xi == 0.) */
	a = -iqr / log(3.);
	for (i = 0; i < n; ++i) {
	    px = unif_rand();
	    x[i] = med + a * log(2. - 2. * px);
	}
	break;

    case 5:
	/* (iqr <= 0. || chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) */
	for (i = 0; i < n; ++i)
	    x[i] = R_NaN;
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
	    px = unif_rand();
	    x[i] = a * (1. - px) + b * px;
	}
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

    /* take care of seed */
    PutRNGstate();

}

#undef DO_LOOP

SEXP
gldist_rgl(SEXP n, SEXP med, SEXP iqr, SEXP chi, SEXP xi) {

    double params[4];
    int len;

    SEXP x;

    /* build params vector */
    gldist_params(params, n, med, iqr, chi, xi);

    /* Either use the value of n if it is of length 1 or its lenght */
    len = length(n);
    if (len == 1)
	len = asInteger(n);
    if (len == NA_INTEGER || len < 0)
	Rf_error("n must be > 0");

    /* create returned object */
    PROTECT(x = allocVector(REALSXP, len));
    if (len == 0) {
	UNPROTECT(1);
	return x;
    }

    /* Compute quantiles */
    gldist_do_rgl(REAL(x), params[0], params[1], params[2], params[3], len);

    UNPROTECT(1);
    return x;

}
