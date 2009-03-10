/**********************************************************************************************
 *   Copyright (c) 2008 Christophe Dutang                                                                    *
 *                                                                                                                                           *
 *   This program is free software; you can redistribute it and/or modify                 *
 *   it under the terms of the GNU General Public License as published by           *
 *   the Free Software Foundation; either version 2 of the License, or                     *
 *   (at your option) any later version.                                                                              *
 *                                                                                                                                           *
 *   This program is distributed in the hope that it will be useful,                               *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 *   GNU General Public License for more details.                                                      *
 *                                                                                                                                           *
 *   You should have received a copy of the GNU General Public License             *
 *   along with this program; if not, write to the                                                             *
 *   Free Software Foundation, Inc.,                                                                                *
 *   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA                               *
 *                                                                                                                                           *
 **********************************************************************************************/
/*
 *  Torus algorithm to generate random numbers
 *  
 *			header file
 *
 */

//R header files
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/Error.h>

#include "config.h"
#include "locale.h"


//SFMT
#include "SFMT.h"

//WELL
#include "WELL512a.h"
#include "WELL521a.h"
#include "WELL521b.h"
#include "WELL607a.h"
#include "WELL607b.h"
#include "WELL1024a.h"
#include "WELL1024b.h"

#include "WELL800a.h"
#include "WELL800aTemp.h"
#include "WELL800b.h"
#include "WELL800bTemp.h"
#include "WELL19937a.h"
#include "WELL19937aTemp.h"
#include "WELL19937b.h"
#include "WELL19937bTemp.h"
#include "WELL21701a.h"
#include "WELL21701aTemp.h"
#include "WELL23209a.h"
#include "WELL23209aTemp.h"
#include "WELL23209b.h"
#include "WELL23209bTemp.h"
#include "WELL44497a.h"
#include "WELL44497aTemp.h"

//Knuth TAOCP
#include "knuthTAOCP2002.h"


//time header files
#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_WINDOWS_H
# include <windows.h>
#endif

#if defined(HAVE_SSE2)
# include <emmintrin.h>
#endif


/* Functions accessed from .Call() */
SEXP doTorus(SEXP n, SEXP d, SEXP p, SEXP offset, SEXP ismixed, SEXP timedseed);
SEXP doSetSeed(SEXP s);
SEXP doCongruRand(SEXP n, SEXP d, SEXP modulus, SEXP multiplier, SEXP increment, SEXP echo);
SEXP doSFMersenneTwister(SEXP n, SEXP d, SEXP mersexpo, SEXP paramset);
SEXP doWELL(SEXP n, SEXP d, SEXP order, SEXP tempering, SEXP version);
SEXP doKnuthTAOCP(SEXP n, SEXP d);

/* utility functions */
void torus(double *u, int nb, int dim, int *prime, int offset, int ismixed, int usetime);
void congruRand(double *u, int nb, int dim, unsigned long long mod, unsigned long long mult, unsigned long long incr, int show);
void SFmersennetwister(double *u, int nb, int dim, int mexp, int usepset);
void WELLrng(double *u, int nb, int dim, int order, int temper, int version);
void knuthTAOCP(double *u, int nb, int dim);

void setSeed(long s);
void randSeedByArray(int length);
void randSeed();

void reconstruct_primes();
void get_primes(int *n, int *pri);

