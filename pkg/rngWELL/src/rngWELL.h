/**********************************************************************************************
 *   Copyright (c) 2009 Christophe Dutang and Petr Savicky                                    *
 *                                                                                            *
 *    This code can be used freely for personal, academic, or non-commercial purposes.        *
 *    For commercial purposes, please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca        *                                                          *
 *                                                                                            *                                                                                                 *
 **********************************************************************************************/
/*
 *  WELL generators
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
SEXP doSetSeed4WELL(SEXP s);
SEXP doWELL(SEXP n, SEXP d, SEXP order, SEXP tempering, SEXP version);

/* utility functions */
void WELLrng(double *u, int nb, int dim, int order, int temper, int version);

void setSeed4WELL(long s);
void randSeedByArray(int length);
void randSeed();
