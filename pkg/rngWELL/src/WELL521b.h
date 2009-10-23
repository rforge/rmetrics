/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL521 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL521, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL521b_H
#define WELL521b_H

void InitWELLRNG521b (unsigned int *init);
extern double (*WELLRNG521b)(void);

void GetWELLRNG521b (unsigned int *state);
#endif

