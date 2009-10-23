/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL800 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL800, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL800a_H
#define WELL800a_H

void InitWELLRNG800a (unsigned int *init);
extern double (*WELLRNG800a)(void);

void GetWELLRNG800a (unsigned int *state);
#endif

