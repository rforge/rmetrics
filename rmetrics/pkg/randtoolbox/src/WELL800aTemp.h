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

#ifndef WELL800aTemp_H
#define WELL800aTemp_H

void InitWELLRNG800aTemp (unsigned int *init);
extern double (*WELLRNG800aTemp)(void);

#endif

