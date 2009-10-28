/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL44497b is __entirely__ based on the code of WELL44497a by P. L'Ecuyer.
 * we just change constants, parameters to get WELL44497b, add some
 * code to interface with R and add some comments on #define's.
 */

#ifndef WELL44497aTemp_H
#define WELL44497aTemp_H

void InitWELLRNG44497aTemp(unsigned int *);
extern double (*WELLRNG44497aTemp)(void);

void GetWELLRNG44497aTemp (unsigned int *state);
#endif

