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

#define DO_LOOP								\
    px = 1.;								\
    for (i = 0; i < n; ++i) {						\
	xx = *ptr[i];							\
	if (ISNAN(xx)) {						\
	    dx = xx;							\
	} else if (xx == qmin) {					\
	    px = 0.;							\
	    dS = QDSFUN;						\
	    dx = 1. / (e * dS);						\
	} else if (xx == qmax) {					\
	    px = 1.;							\
	    dS = QDSFUN;						\
	    dx = 1. / (e * dS);						\
	} else if (xx < qmin || xx > qmax) {				\
	    dx = 0.;							\
	} else {							\
	    /* To avoid troulbes in newton steps*/			\
	    if (ISNAN(px) || px > ONEMEPS)				\
		px = 1.;						\
	    maxiter = maxit;						\
	    y = (xx - a) / b;						\
	    px = gldist_zeroin_Newton(y, 0., px, SFUN, DSFUN,		\
				      INFO, 0., &maxiter);		\
	    if (maxiter < 0) Rf_warning("Reached maxit for x=%f", xx);	\
	dS = QDSFUN;							\
	dx = 1 / (e * dS);						\
	}								\
	d[ptr[i] - x] = dx;						\
    }

void
gldist_do_dgl(double *d, double * const x, double med, double iqr,
              double chi, double xi, int n, int maxit) {

    /* Note try to limit as much as possible logical test in the for loop */
    /* more over we use seperate variables to help compilers to
       auto-vectorize the calls to mathematical functions. */

    double * p;
    double px, xx, dx;
    double *qrange = (double*) R_alloc(2, sizeof(double));
    double qmin, qmax;
    double prange[2] = {0., 1.};
    double alpha, beta;
    double Sv[2], pv[2], ev[2];
    double Sqv[3], qv[3] = {.25, .5, .75};
    double a, b, c, e;
    double S, dS;
    double y;
    double * pars = (double*) R_alloc(3, sizeof(double));
    int flag = 0;
    int i, maxiter;
    double **ptr;

    WHICH_CASE(flag, med, iqr, chi, xi);

    switch (flag) {

    case 1:
	/* (ISNA(med) || ISNA(iqr) || ISNA(chi) || ISNA(xi)) */
	for (i = 0; i < n; ++i)
	    d[i] = NA_REAL;
	break;

    case 2:
	/* (ISNAN(med) || ISNAN(iqr) || ISNAN(chi) || ISNAN(xi)) */
	for (i = 0; i < n; ++i)
	    d[i] = R_NaN;
	break;

    case 3:
	/* (chi == -1. && xi == 0.) */
	c = log(3.);
	a = med + iqr * log(2.) / c;
	b = iqr / c;
	e = log(3.) / iqr;
        qmin = -INFINITY;
        qmax = a;
	for (i = 0; i < n; ++i) {
	    xx = x[i];
	    if (ISNAN(xx)) {
		dx = xx;
	    } else if (xx == qmax) {
		dx = e;
	    } else if (xx <= qmin || xx > qmax) {
		dx = 0.;
	    } else {
		y = (xx - a) / b;
		px = exp(y);
		dx = e * px;
	    }
	    d[i] = dx;
	}
	break;

    case 4:
	/* (chi == 1. && xi == 0.) */
	c = log(3.);
	a = med - iqr * log(2.) / c;
	b = - iqr / c;
	e = log(3.) / iqr;
        qmin = a;
        qmax = INFINITY;
	for (i = 0; i < n; ++i) {
	    xx = x[i];
	    if (ISNAN(xx)) {
		dx = xx;
	    } else if (xx == qmin) {
		dx = e;
	    } else if (xx < qmin || xx >= qmax) {
		dx = 0.;
	    } else {
		y = (xx - a) / b;
		px = 1. - exp(y);
		dx = e * (1. - px);
	    }
	    d[i] = dx;
	}
	break;

    case 5:
	/* (iqr <= 0. || chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) */
	for (i = 0; i < n; ++i)
	    d[i] = R_NaN;
	Rf_warning("NaNs produced");
	break;

    case 6:
	/* (chi == 0 && xi == .5) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = x + i;
	qsort(ptr, n, sizeof(double*), cmp);
	for (i = 0; i < 3; i++)
	    Sqv[i]  = gldist_do_S1(qv[i], (void *) NULL);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
	e = iqr / (Sqv[2] - Sqv[0]);
        qmin = -INFINITY;
        qmax = INFINITY;
#define SFUN &gldist_do_S1
#define DSFUN &gldist_do_dS1
#define INFO ((void *) NULL)
#define QDSFUN gldist_do_dS1(px, (void *) NULL)
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
#undef  QDSFUN
	break;

    case 7:
	/* (chi != 0 && xi == .5 * (1. + chi)) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = x + i;
	qsort(ptr, n, sizeof(double*), cmp);
	alpha = (.5 - xi) / sqrt(xi * (1. - xi));
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S2(qv[i], &alpha);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
	e = iqr / (Sqv[2] - Sqv[0]);
        qmin = -INFINITY;
        qmax = (alpha > 0) ? a + b / alpha : INFINITY;
#define SFUN &gldist_do_S2
#define DSFUN &gldist_do_dS2
#define INFO &alpha
#define QDSFUN gldist_do_dS2(px, &alpha)
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
#undef  QDSFUN
	break;

    case 8:
	/* (chi != 0 && xi == .5 * (1. - chi)) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = x + i;
	qsort(ptr, n, sizeof(double*), cmp);
	beta = chi / sqrt(1. - chi * chi);
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S3(qv[i], &beta);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
	e = iqr / (Sqv[2] - Sqv[0]);
        qmin = (beta > 0) ? a - b / beta : -INFINITY;
        qmax = INFINITY;
#define SFUN &gldist_do_S3
#define DSFUN &gldist_do_dS3
#define INFO &beta
#define QDSFUN gldist_do_dS3(px, &beta)
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
#undef  QDSFUN
	break;

    case 9:
	/* (chi == 0 && (xi == XIUNIF1 || xi == XIUNIF2)) */
	a = med - iqr;
	b = med + iqr;
	c = 1. / (b - a);
	for (i = 0; i < n; ++i) {
	    xx = x[i];
	    if (ISNAN(xx)) {
		dx = xx;
	    } else if (xx < a) {
		dx = 0.;
	    } else if (xx > b) {
		dx = 0.;
	    } else {
		dx = c;
	    }
	    d[i] = dx;
	}
	break;

    default:
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = x + i;
	qsort(ptr, n, sizeof(double*), cmp);
        alpha = .5 * (.5 - xi) / sqrt(xi * (1. - xi));
        beta  = .5 * chi / sqrt(1. - chi * chi);
        ev[0] = alpha + beta;
        ev[1] = alpha - beta;
        for (i = 0; i < 3; i++)
            Sqv[i]  = gldist_do_S4(qv[i], ev);
        c = Sqv[2] - Sqv[0];
        a = med - iqr * Sqv[1] / c;
        b = iqr / c;
	e = iqr / (Sqv[2] - Sqv[0]);
        qmin = (ev[0] > 0) ? a - b / ev[0] : -INFINITY;
        qmax = (ev[1] > 0) ? a + b / ev[1] : INFINITY;
#define SFUN &gldist_do_S4
#define DSFUN &gldist_do_dS4
#define INFO ev
#define QDSFUN gldist_do_dS4(px, ev)
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
#undef  QDSFUN
	break;

    }
}

#undef DO_LOOP

SEXP
gldist_dgl(SEXP x, SEXP med, SEXP iqr, SEXP chi, SEXP xi, SEXP maxit) {

    double params[4];
    int len;

    SEXP d;

    /* build parameters vector */
    gldist_params(params, x, med, iqr, chi, xi);
    /* explicitly convert to a real vector to catch non-numeric NAs */
    x = coerceVector(x, REALSXP);

    /* create returned object */
    len = length(x);
    PROTECT(d = allocVector(REALSXP, len));

    /* Compute probabilities */
    gldist_do_dgl(REAL(d), REAL(x), params[0], params[1],
		  params[2], params[3],
                  len, asInteger(maxit));

    UNPROTECT(1);
    return d;

}

