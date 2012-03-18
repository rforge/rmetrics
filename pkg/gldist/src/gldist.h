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

/*******************************/
/* common defines              */
/*******************************/

#define EPS DBL_EPSILON
#define ONEMEPS (1 - 2 * DBL_EPSILON)
#define XIUNIF1 (.5 - 1./sqrt(5.))
#define XIUNIF2 (.5 - 2./sqrt(17.))

#define WHICH_CASE(flag, med, iqr, chi, xi)				\
    if (ISNA(med) || ISNA(iqr)) {					\
	flag = 1;							\
    } else if (ISNAN(med) || ISNAN(iqr)) {				\
	flag = 2;							\
    } else if (iqr <= 0) {						\
        flag = 5;							\
    } else if (ISNA(chi) || ISNA(xi)) {					\
	flag = 1;							\
    } else if (ISNAN(chi) || ISNAN(xi)) {				\
	flag = 2;							\
    } else if (chi == -1. && xi == 0.) {				\
        flag = 3;							\
    } else if (chi ==  1. && xi == 0.) {				\
        flag = 4;							\
    } else if (chi <= -1. || chi >= 1. || xi <= 0. || xi >= 1. ) {	\
        flag = 5;							\
    } else if (chi == 0 && xi == .5) {					\
        flag = 6;							\
    } else if (xi == .5 * (1. + chi)) {					\
        flag = 7;							\
    } else if (xi == .5 * (1. - chi)) {					\
        flag = 8;							\
    } else if (chi == 0 && (xi == XIUNIF1 || xi == XIUNIF2)) {		\
        flag = 9;							\
    }

/*******************************/
/* qgl.c                       */
/*******************************/

double
gldist_do_S1(double x, void *info);

double
gldist_do_S2(double x, void *info);

double
gldist_do_S3(double x, void *info);

double
gldist_do_S4(double x, void *info);

void
gldist_do_qgl(double *q, double * const p, double med, double iqr, double chi, double xi, int n);

SEXP
gldist_qgl(SEXP p, SEXP med, SEXP iqr, SEXP xi, SEXP chi);

/*******************************/
/* qdgl.c                      */
/*******************************/

double
gldist_do_dS1(double x, void *info);

double
gldist_do_dS2(double x, void *info);

double
gldist_do_dS3(double x, void *info);

double
gldist_do_dS4(double x, void *info);

void
gldist_do_qdgl(double *q, double * const p, double med, double iqr, double chi, double xi, int n);

SEXP
gldist_qdgl(SEXP p, SEXP med, SEXP iqr, SEXP xi, SEXP chi);

/*******************************/
/* rgl.c                       */
/*******************************/

void
gldist_do_rgl(double *x, double med, double iqr, double chi, double xi, int n);

SEXP
gldist_qdgl(SEXP n, SEXP med, SEXP iqr, SEXP xi, SEXP chi);


/*******************************/
/* pgl.c                       */
/*******************************/

int
cmp(const void *a, const void *b);

double
gldist_zeroin_Newton(double y, double a, double b, double (*f)(double x, void *info),
		     double (*g)(double x, void *info), void *info,
		     double tol, int *Maxit);

double
rootf(double x, void *info);

double
rootg(double x, void *info);

void
gldist_do_pgl(double *p, double * const q, double med, double iqr,
	      double chi, double xi, int n, int maxiter);

SEXP
gldist_pgl(SEXP q, SEXP med, SEXP iqr, SEXP chi, SEXP xi, SEXP maxit);

/*******************************/
/* params.c                       */
/*******************************/

void
gldist_params(double * params, SEXP arg1, SEXP med, SEXP iqr, SEXP chi, SEXP xi);
