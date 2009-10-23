/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL1024a_H
#define WELL1024a_H

void InitWELLRNG1024a (unsigned int *init);
double WELLRNG1024a (void);

void GetWELLRNG1024a (unsigned int *state);
#endif

