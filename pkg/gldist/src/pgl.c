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
gldist_zeroin_Newton(double y, double a, double b, double (*f)(double x, void *info),
		     double (*g)(double x, void *info), void *info,
		     double tol, int *Maxit) {

    double fa, fb;
    double c, fc;
    double p, q;
    double new_step;
    double tol_act;
    int maxit = *Maxit + 1;

    if (ISNAN(y))
	return y;

    fa = (*f)(a, info) - y;
    fb = (*f)(b, info) - y;

    /* test if we have found a root at an endpoint */
    if(fabs(fa) < 2 * EPS) {
        *Maxit = 0;
        return a;
    }
    if(fabs(fb) < 2 * EPS) {
        *Maxit = 0;
        return b;
    }

    /* if there is no root in the interval use the largest possible
       interval. If the root still does not exists, returns NaN */
    if (fa * fb > 0) {
	a = 0.;
	b = 1.;
	fa = (*f)(a, info) - y;
	fb = (*f)(b, info) - y;
	if (fa * fb > 0) return(R_NaN);
    }

    while (maxit--) {

        /* swap data to have b the closest to the root */
        if (fabs(fa) < fabs(fb)) {
            c = a; a = b; b = c;
            fc = fa; fa = fb; fb = fc;
        }

	/* bisection step */
        new_step = .5 * (a - b);

	/* Newton steps */
	p = fb;
        q = (*g)(b, info);

	/* keep p positive for Newton step conditions */
        if (p > 0.) {
            q = -q;
	} else {
	    p = -p;
	}

	if ((fabs(q) != INFINITY) &&
	    (q != 0.) &&
	    p < fabs(new_step * q)) {
	    new_step = p/q;
	}

	/* save last iteration */
	c = b; fc = fb;
	b += new_step;
	fb = (*f)(b, info) - y;

        tol_act = 2. * EPS * fabs(b) + tol / 2.;
	if (fabs(new_step) <= tol_act || fb == 0) {
            *Maxit -= maxit;
            return b;
	}

	/* reduce search interval */
	if ((fb * fc) < 0) {
            a = c; fa = fc;
        }

    }

    /* failed! */
    *Maxit = -1;

    return b;
}

int
cmp(const void *a, const void *b)
{
    const double *aa = *(double**)a;
    const double *bb = *(double**)b;
    int naa = ISNAN(*aa);
    int nab = ISNAN(*bb);
    if (naa && nab) return(0);
    if (naa) return(1);
    if (nab) return(-1);
    return (*aa < *bb) - (*aa > *bb);
}

#define DO_LOOP								\
    px = 1.;								\
    for (i = 0; i < n; ++i) {						\
	qx = *ptr[i];							\
	if (ISNAN(qx)) {						\
	    px = qx;							\
	} else if (qx <= qmin) {					\
	    px = 0.;							\
	} else if (qx >= qmax) {					\
	    px = 1.;							\
	} else {							\
	    /* To avoid troulbes in newton steps*/			\
	    if (ISNAN(px) || px > ONEMEPS)				\
		px = 1.;						\
	    maxiter = maxit;						\
	    y = (qx - a) / b;						\
	    px = gldist_zeroin_Newton(y, 0., px, SFUN, DSFUN,		\
				      INFO, 0., &maxiter);		\
	    if (maxiter < 0) Rf_warning("Reached maxit for q=%f", qx);	\
	}								\
	p[ptr[i] - q] = px;						\
    }

void
gldist_do_pgl(double *p, double * const q, double med, double iqr,
              double chi, double xi, int n, int maxit) {

    /* Note try to limit as much as possible logical test in the for loop */
    /* more over we use seperate variables to help compilers to
       auto-vectorize the calls to mathematical functions. */

    double px, qx;
    double qmin, qmax;
    double alpha, beta;
    double ev[2];
    double Sqv[3], qv[3] = {.25, .5, .75};
    double a, b, c;
    double y;
    int flag = 0;
    int i, maxiter;
    double **ptr;

    WHICH_CASE(flag, med, iqr, chi, xi);

    switch (flag) {

    case 1:
	/* (ISNA(med) || ISNA(iqr) || ISNA(chi) || ISNA(xi)) */
	for (i = 0; i < n; ++i)
	    p[i] = NA_REAL;
	break;

    case 2:
	/* (ISNAN(med) || ISNAN(iqr) || ISNAN(chi) || ISNAN(xi)) */
	for (i = 0; i < n; ++i)
	    p[i] = R_NaN;
	break;

    case 3:
	/* (chi == -1. && xi == 0.) */
	qmin = -INFINITY;
	qmax = med + iqr * log(2.)/log(3.);
	for (i = 0; i < n; ++i) {
	    qx = q[i];
	    if (ISNAN(qx)) {
		px = qx;
	    } else if (qx <= qmin) {
		px = 0.;
	    } else if (qx >= qmax) {
		px = 1.;
	    } else {
		px = .5 * pow(3., (qx - med)/iqr);
	    }
	    p[i] = px;
	}
	break;

    case 4:
	/* (chi == 1. && xi == 0.) */
	qmin = med - iqr * log(2.)/log(3.);
	qmax = INFINITY;
	for (i = 0; i < n; ++i) {
	    qx = q[i];
	    if (ISNAN(qx)) {
		px = qx;
	    } else if (qx <= qmin) {
		px = 0.;
	    } else if (qx >= qmax) {
		px = 1.;
	    } else {
		px = 1. - .5 * pow(3., (med - qx)/iqr);
	    }
	    p[i] = px;
	}
	break;

    case 5:
	/* (iqr <= 0. || chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) */
	for (i = 0; i < n; ++i)
	    p[i] = R_NaN;
	Rf_warning("NaNs produced");
	break;

    case 6:
	/* (chi == 0 && xi == .5) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = q + i;
	qsort(ptr, n, sizeof(double*), cmp);
	for (i = 0; i < 3; i++)
	    Sqv[i]  = gldist_do_S1(qv[i], (void *) NULL);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
        qmin = -INFINITY;
        qmax = INFINITY;
#define SFUN &gldist_do_S1
#define DSFUN &gldist_do_dS1
#define INFO ((void *) NULL)
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
	break;

    case 7:
	/* (xi != 0 && xi == .5 * (1. + chi)) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = q + i;
	qsort(ptr, n, sizeof(double*), cmp);
	alpha = (.5 - xi) / sqrt(xi * (1. - xi));
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S2(qv[i], &alpha);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
        qmin = -INFINITY;
        qmax = (alpha > 0) ? a + b / alpha : INFINITY;
#define SFUN &gldist_do_S2
#define DSFUN &gldist_do_dS2
#define INFO &alpha
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
	break;

    case 8:
	/* (xi != 0 && xi == .5 * (1. - chi)) */
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = q + i;
	qsort(ptr, n, sizeof(double*), cmp);
	beta = chi / sqrt(1. - chi * chi);
	for (i = 0; i < 3; i++)
	    Sqv[i] = gldist_do_S3(qv[i], &beta);
	c = Sqv[2] - Sqv[0];
	a = med - iqr * Sqv[1] / c;
	b = iqr / c;
        qmin = (beta > 0) ? a - b / beta : -INFINITY;
        qmax = INFINITY;
#define SFUN &gldist_do_S3
#define DSFUN &gldist_do_dS3
#define INFO &beta
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
	break;

    case 9:
	/* (chi == 0 && (xi == XIUNIF1 || xi == XIUNIF2)) */
	a = med - iqr;
	b = med + iqr;
	c = 1 / (b - a);
	for (i = 0; i < n; ++i) {
	    qx = q[i];
	    if (ISNAN(qx)) {
		px = qx;
	    } else if (qx < a) {
		px = 0.;
	    } else if (qx >= b) {
		px = 1.;
	    } else {
		px = c * (qx - a);
	    }
	    p[i] = px;
	}
	break;

    default:
	ptr = (double **) R_alloc(n, sizeof(double*));
	for (i = 0; i < n; ++i)
	    ptr[i] = q + i;
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
        qmin = (ev[0] > 0) ? a - b / ev[0] : -INFINITY;
        qmax = (ev[1] > 0) ? a + b / ev[1] : INFINITY;
#define SFUN &gldist_do_S4
#define DSFUN &gldist_do_dS4
#define INFO ev
	DO_LOOP
#undef  SFUN
#undef  DSFUN
#undef  INFO
	break;

    }
}

#undef DO_LOOP

SEXP
gldist_pgl(SEXP q, SEXP med, SEXP iqr, SEXP chi, SEXP xi, SEXP maxit) {

    double params[4];
    int len;

    SEXP p;

    /* build parameters vector */
    gldist_params(params, q, med, iqr, chi, xi);
    /* explicitly convert to a real vector to catch non-numeric NAs */
    q = coerceVector(q, REALSXP);

    /* create returned object */
    len = length(q);
    PROTECT(p = allocVector(REALSXP, len));

    /* Compute probabilities */
    gldist_do_pgl(REAL(p), REAL(q), params[0], params[1],
		  params[2], params[3],
                  len, asInteger(maxit));

    UNPROTECT(1);
    return p;

}
