/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL21701 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL21701, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL21701a_H
#define WELL21701a_H

void InitWELLRNG21701a(unsigned int *);
extern double (*WELLRNG21701a)(void);

#endif

