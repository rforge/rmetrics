/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL19937a_H
#define WELL19937a_H

void InitWELLRNG19937a (unsigned int *);
extern double (*WELLRNG19937a) (void);

void GetWELLRNG19937a (unsigned int *);
#endif

