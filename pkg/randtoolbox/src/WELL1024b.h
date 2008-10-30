/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL1024b is __entirely__ based on the code of WELL1024a by P. L'Ecuyer.
 * we just change constants, parameters to get WELL1024b, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL1024b_H
#define WELL1024b_H

void InitWELLRNG1024b (unsigned int *init);
double WELLRNG1024b (void);

#endif

