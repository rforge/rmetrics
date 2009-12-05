/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL19937c_H
#define WELL19937c_H

void InitWELLRNG19937c (unsigned int *);
extern double (*WELLRNG19937c) (void);

void GetWELLRNG19937c (unsigned int *);
#endif

