/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL23209 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL23209, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL23209aTemp_H
#define WELL23209aTemp_H

void InitWELLRNG23209aTemp (unsigned int *init);
extern double (*WELLRNG23209aTemp)(void);

#endif
