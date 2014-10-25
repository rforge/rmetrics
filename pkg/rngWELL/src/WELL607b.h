/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL607 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL607, add some
 * code to interface with R and add some comments on #define's.
 */


#ifndef WELL607b_H
#define WELL607b_H

void InitWELLRNG607b (unsigned int *init);
extern double (*WELLRNG607b)(void);

void GetWELLRNG607b (unsigned int *state);
#endif

