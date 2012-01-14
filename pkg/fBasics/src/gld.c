/* gld.fm5.fx.c - Part of Robert King's gld package for the R statistical
 * language.
 *
 * Copyright (C) Robert King 1993,2000,2001,2005,2006
 * robert.king@newcastle.edu.au
 * http://tolstoy.newcastle.edu.au/~rking/publ/software.html
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
 *
 * These functions calculate F(x) for the five parameter version of the
 * FMKL paramterisation of the gld.
 * This parameterisation is a variation of the 5 parameter version of the
 * RSparameterisation.
 *
 * This is not very good C.  It is based on gld.rs.fx.c, which is based on
 * code written in 1993, as my first real program.  I still haven't tidied
 * it up very much for release with the R package.
 * However, it has been used for some time and works fine.
 *
 * This is code is loaded into R.
 * The two include lines are handled by R, but need to be here to keep
 * the compile step happy
 *
 * $Id$
 */

#include <math.h>
#include <R.h>

/* proto */

void fm5_funcd( double , double , double *, double *, double *, double *, double *,
double *, double *);
/* This needs a comment for explaining the arguments */

void gl_fm5_distfunc( double *pa,double *pb,double *pc,double *pd, double *pe,
double  *pu1,double *pu2,double *pxacc, int *max_it,
double *ecks, double *u, int *lengthofdata)
{


/* pa to pe:    pointers to the values of the parameters of the gld (fm5 param)
 * pu1:         minimum value of u, should be zero
 * pu2:         maximum value of u, should be 1
 * pxacc:       desired accuracy of the calculation
 * max_it:      maximum iterations for N-R root finder
 * ecks:        the quantiles of the gld given
 * u:           array to put the calculated depths
 * pl:          length of the data
 */

double  u1, u2, xacc;

int i,j;
double df,dx,dxold,f,fh,fl;
double x;
double temp,xh,xl,rts;
/* trying initialising things */
i=0; j=0;
df=0.0;dx=0.0;dxold=0.0;f=0.0;fh=0.0;fl=0.0;
x=0.0;
temp=0.0;xh=0.0;xl=0.0;rts=0.0;

u1 = *pu1; u2 = *pu2; xacc = *pxacc;

/* If we are trying to do things outside the range, lets set them at almost 0 or almost 1 */
if (*pc < 0) {
	if (u1 == 0) {
		u1 = xacc;
		}
	if (u2 == 1) {
		u2 = 1-xacc;
		}
	}
if (*pd < 0) {
	if (u1 == 0) {
		u1 = xacc;
		}
	if (u2 == 1) {
		u2 = 1-xacc;
		}
	}


for (i=0;i<*lengthofdata;i++)
{
    x = ecks[i];
	u[i] = 0.0;
	fm5_funcd(u1,x,&fl,&df,pa,pb,pc,pd,pe);
	fm5_funcd(u2,x,&fh,&df,pa,pb,pc,pd,pe);
	if (fl*fh >= 0.0)
	{
		REprintf("Program aborted at parameter values %f, %f, %f, %f %f\n", *pa, *pb, *pc, *pd, *pe);
		REprintf("The data value being investigated was index %d",i);
		REprintf(" value: %f\n",x);
		error("C code failure - see error message printed above");
	}
	if (fl < 0.0) {
		xl = u1;
		xh = u2;
		}
	else {
		xh = u1;
		xl = u2;
		}
	rts = 0.5*(u1+u2);
	dxold = fabs(u2-u1);
	dx = dxold;
	fm5_funcd(rts,x,&f,&df,pa,pb,pc,pd,pe);
	for (j=1;j<=*max_it;j++) {
		if ((((rts - xh)*df - f)* ( (rts-xl)*df - f) >= 0.0 ) || ( fabs(2.0*f) > fabs (dxold*df))) {
			dxold = dx;
			dx = .5* (xh - xl);
			rts = xl +dx;
			if (xl == rts ) {
				u[i] = rts;
				break; }
			}
		else {
			dxold = dx;
			dx = f/df;
			temp = rts;
			rts -= dx;
			if (temp == rts) {
				u[i] = rts;
				break; }
			}
		if (fabs(dx) < xacc) {
			u[i] = rts;
			break; }
		fm5_funcd(rts,x,&f,&df,pa,pb,pc,pd,pe);
		if (f < 0.0)
			xl =rts;
		else
			xh =rts;
		}
}
}


void fm5_funcd( double u, double x, double *F, double *dFdu, double *pa, double *pb, double *pc, double *pd, double *pe)
{

/* *F is the gld F-1(u)  */
/* *dFdu is dF-1(u)/du	*/
/* NOTE: *dFdu is 1/f(x) a.k.a. 1/f(F-1(u)) - the opposite of what's
	required for the pdf */

if ( *pc == 0 ) {
	if ( *pd == 0 ) {
		/*	Both l3 and l4 zero 	*/
		*F = *pa + ( (1 - *pe) * log(u) - (1 + *pe) * (log(1.0-u)) )/ *pb - x;
		/* need the derivatives for each case */
		*dFdu = ( (1 - *pe)/u + (1 + *pe)/(1-u)) / *pb  ;
		}
	else {
		/*	l3 zero, l4 non-zero	*/
		*F = *pa + ((1 - *pe) *  log(u) - ((1 + *pe)*( pow((1-u),*pd)-1)/ *pd ) ) / *pb - x;
		*dFdu = ((1 - *pe)/u + (1 + *pe)*pow((1-u), *pd-1) )/ *pb ;
		}
	}
else {
	if ( *pd == 0 ) {
		/*  l4 zero, l3 non-zero    */
		*F = *pa + ( (1 - *pe)*(( pow(u,*pc)- 1)/ *pc) - (1 + *pe)* log(1-u) ) / *pb - x;
		*dFdu = ( (1 - *pe) * pow(u, *pc-1) + (1 + *pe)/(1-u)  ) / *pb;
		}
	else {
		/*  l3 non-zero, l4 non-zero    */
		*F = ((1 - *pe)* ( pow((u),*pc) -1 )/ *pc  - (1 + *pe)* (pow((1.0-u),*pd) -1 )/ *pd )/ *pb + *pa - x;
		*dFdu = ((1 - *pe)* ( pow((u),(*pc-1.0)) ) + (1 + *pe)* ( pow( (1.0-u),(*pd-1.0)) ) )/ *pb ;

		}
	}

}

/* gld.fmkl.fx.c - Part of Robert King's gld package for the R statistical
 * language.
 *
 * Copyright (C) Robert King 1993,2000,2001,2006
 * robert.king@newcastle.edu.au
 * http://tolstoy.newcastle.edu.au/~rking/publ/software.html
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
 *
 * These functions calculate F(x) for the Freimer, Mudholkar, Kollia and Lin parameterisation
 * of the gld.  It is adapted from gld.rs.fx.c (also in this R distribution).
 * Because of that, this is not very good C.  The original code was my first
 * real program in C and I haven't tidied it up very much for release with the R package.
 * However, it has been used for some time and works fine.
 *
 * This is code is loaded into R.
 * The two include lines are handled by R, but need to be here to keep the compile step happy
 * $Id$
 */

#include <stdio.h>
#include <math.h>
#include <R.h>

/* proto */

void fmkl_funcd( double , double , double *, double *, double *, double *, double *, double *);

/* the function that finds the root */

void gl_fmkl_distfunc( double *pa,double *pb,double *pc,double *pd,
double  *pu1,double *pu2,double *pxacc, int *max_it,
double *ecks, double *u, int *lengthofdata)
{

/* pa to pd:    pointers to the values of the parameters of the gld (rs param)
 * pu1:         minimum value of u, should be zero
 * pu2:         maximum value of u, should be 1
 * pxacc:       desired accuracy of the calculation
 * max_it:      maximum iterations for N-R root finder
 * ecks:        the quantiles of the gld given
 * u:           array to put the calculated depths
 * pl:          length of the data
 */

double  u1, u2, xacc;

int i,j;
double df,dx,dxold,f,fh,fl;
double x;
double temp,xh,xl,rts;
/* trying initialising things */
i=0; j=0;
df=0.0;dx=0.0;dxold=0.0;f=0.0;fh=0.0;fl=0.0;
x=0.0;
temp=0.0;xh=0.0;xl=0.0;rts=0.0;

u1 = *pu1; u2 = *pu2; xacc = *pxacc;

if (*pc < 0) {
	if (u1 == 0) {
		u1 = xacc;
		}
	if (u2 == 1) {
		u2 = 1-xacc;
		}
	}
if (*pd < 0) {
	if (u1 == 0) {
		u1 = xacc;
		}
	if (u2 == 1) {
		u2 = 1-xacc;
		}
	}


for (i=0;i<*lengthofdata;i++)
{
    x = ecks[i];
	u[i] = 0.0;
	fmkl_funcd(u1,x,&fl,&df,pa,pb,pc,pd);
	fmkl_funcd(u2,x,&fh,&df,pa,pb,pc,pd);
	if (fl*fh >= 0.0)
	{
		/* This is suggested in writing R extensions, but still gives the warning */
		/* error("Program aborted at parameter values %f, %f, %f, %f\n The data value being investigated was index %d, value: %f\n", *pa, *pb, *pc, *pd, i, x); */
		REprintf("C code aborted at parameter values %f, %f, %f, %f\n", *pa, *pb, *pc, *pd);
		REprintf("The data value being investigated was index %d",i);
		REprintf(" value: %f\n",x);
		error("C code numerical failure");
	}
	if (fl < 0.0) {
		xl = u1;
		xh = u2;
		}
	else {
		xh = u1;
		xl = u2;
		}
	rts = 0.5*(u1+u2);
	dxold = fabs(u2-u1);
	dx = dxold;
	fmkl_funcd(rts,x,&f,&df,pa,pb,pc,pd);
	for (j=1;j<=*max_it;j++) {
		if ((((rts - xh)*df - f)* ( (rts-xl)*df - f) >= 0.0 ) ||
( fabs(2.0*f) > fabs (dxold*df))) {
			dxold = dx;
			dx = .5* (xh - xl);
			rts = xl +dx;
			if (xl == rts ) {
				u[i] = rts;
				break; }
			}
		else {
			dxold = dx;
			dx = f/df;
			temp = rts;
			rts -= dx;
			if (temp == rts) {
				u[i] = rts;
				break; }
			}
		if (fabs(dx) < xacc) {
			u[i] = rts;
			break; }
		fmkl_funcd(rts,x,&f,&df,pa,pb,pc,pd);
		if (f < 0.0)
			xl =rts;
		else
			xh =rts;
		}
}
}


void fmkl_funcd( double u, double x, double *F, double *dFdu, double *pa, double *pb, double *pc, double *pd)
{

/* *F is the gld F-1(u)  */
/* *dFdu is dF-1(u)/du	*/
/* NOTE: *dFdu is 1/f(x) a.k.a. 1/f(F-1(u)) - the opposite of what's
	required for the pdf */

if ( *pc == 0 ) {
	if ( *pd == 0 ) {
		/*	Both l3 and l4 zero 	*/
		*F = *pa + (log(u) - log(1.0-u))/ *pb - x;
		*dFdu = ( 1 /(u * (1-u))) / *pb  ;  /* correct but confusing, should be 1/u + 1/(1-u) */
		}
	else {
		/*	l3 zero, l4 non-zero	*/
		*F = *pa + ( log(u) - (( pow((1-u),*pd)-1)/ *pd ) ) / *pb - x;
		*dFdu = (1/u + pow((1-u), *pd-1) )/ *pb ;
		}
	}
else {
	if ( *pd == 0 ) {
		/*  l4 zero, l3 non-zero    */
		*F = *pa + ( (( pow(u,*pc)- 1)/ *pc) - log(1-u) ) / *pb - x;
		*dFdu = (pow(u, *pc-1) + 1/(1-u)  ) / *pb;
		}
	else {
		/*  l3 non-zero, l4 non-zero    */
		*F = ( ( pow((u),*pc) -1 )/ *pc  - (pow((1.0-u),*pd) -1 )/ *pd )/ *pb + *pa - x;
		*dFdu = ( ( pow((u),(*pc-1.0)) ) + ( pow( (1.0-u),(*pd-1.0)) ) )/ *pb ;

		}
	}

}

/* gld.rs.fx.c - Part of Robert King's gld package for the R statistical
 * language.
 *
 * Copyright (C) Robert King 1993,2000,2001
 * robert.king@newcastle.edu.au
 * http://maths.newcastle.edu.au/~rking/publ/software.html
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA  02111-1307  USA
 *
 * These functions calculate F(x) for the rs paramterisation for the gld
 * This is not very good C.  I wrote this in 1993, as my first real program
 * and I haven't tidied it up very much for release with the R package.
 * However, it has been used for some time and works fine.
 *
 * This is code is loaded into R.  It doesn't need stdio.h
 * It needs math.h, which is loaded automagically by R.
 */

#include <stdio.h>
#include <math.h>
#include <R.h>

static double la, lb, lc, ld, x;

/* the function that finds the root */

void gl_rs_distfunc( double *pa,double *pb,double *pc,double *pd,
double  *px1,double *px2,double *pxacc, int *max_it,
double *ecks, double *u, int *pl)
{

/* pa to pd: 	pointers to the values of the parameters of the gld (rs param)
 * px1:		minimum value of u, should be zero
 * px2:		maximum value of u, should be 1
 * pxacc:	desired accuracy of the calculation
 * max_it: 	maximum iterations for N-R root finder
 * ecks:	the quantiles of the gld given
 * u:		array to put the calculated depths
 * pl:		length of the data
 */

	double  x1, x2, xacc;
	double  a, b, c, d;
	int l;

	int i,j;
	double df,dx,dxold,f,fh,fl;
	double temp,xh,xl,rts;
	void funcd();

	x1 = *px1; x2 = *px2; xacc = *pxacc;
	a = *pa; b = *pb; c = *pc; d = *pd;
	l = *pl;

	la = a; lb = b; lc = c; ld = d;

/* The C version has something here to force the limits to be xacc and 1-xacc
	rather than 0 and 1 if lambda3 and lambda4 are negative.  I can't
	see why, so I'm leaving it out.*/

for (i=0;i<l;i++)
{
        x = ecks[i];
	u[i] = 0.0;
	funcd(x1,&fl,&df);
	funcd(x2,&fh,&df);
	if (fl*fh >= 0.0) {
		REprintf("Program aborted during calculation of F(x)");
		REprintf("at parameter values %f, %f, %f, %f\n", *pa, *pb, *pc, *pd);
		REprintf("The x value being investigated was index: %d",i);
		REprintf(" value: %f\n",x);
		error("C code numerical failure");

		}
	if (fl < 0.0) {
		xl = x1;
		xh = x2;
		}
	else {
		xh = x1;
		xl = x2;
		}
	rts = 0.5*(x1+x2);
	dxold = fabs(x2-x1);
	dx = dxold;
	funcd(rts,&f,&df);
	for (j=1;j<= *max_it;j++) {
		if ((((rts - xh)*df - f)* ( (rts-xl)*df - f) >= 0.0 ) ||
( fabs(2.0*f) > fabs (dxold*df))) {
			dxold = dx;
			dx = .5* (xh - xl);
			rts = xl +dx;
			if (xl == rts ) { u[i] = rts; break; }
			}
		else {
			dxold = dx;
			dx = f/df;
			temp = rts;
			rts -= dx;
			if (temp == rts) { u[i] = rts; break; }
			}
		if (fabs(dx) < xacc) { u[i] = rts; break; }
		funcd(rts,&f,&df);
		if (f < 0.0)
			xl =rts;
		else
			xh =rts;
		}
}
}


void funcd(u,a,b)
double u,*a,*b;
{

/* function is the gld F-1(u)  */
/* df_du is dF-1(u)/du  */

double function, df_du;

if ( lc == 0 ) {
        if ( ld == 0 ) {
                /*      Both l3 and l4 zero     */
                function = la - x;
                df_du = 0;
                }
        else {
                /*      l3 zero, l4 non-zero    */
                function = la + ( 1 - pow((1-u),ld) )/ lb - x;
                df_du = (ld) * ( pow((1.0-u),(ld - 1.0)) / lb);
                }
        }
else {
        if ( ld == 0 ) {
                /*  l4 zero, l3 non-zero    */
                function = la + ( pow(u,lc) - 1 ) / lb - x;
                df_du = lc * ( pow(u,(lc- 1.0)) ) / lb;
                }
        else {
                /*  l3 non-zero, l4 non-zero    */
                function = ( pow((u),lc) - pow((1.0-u),ld) )/ lb + la - x;
                df_du = (lc * (pow((u),(lc-1.0))) + ld * ( pow((1.0-u),(ld-1.0)) ))/ lb;
                }
        }


*a = function;
*b = df_du;

}

