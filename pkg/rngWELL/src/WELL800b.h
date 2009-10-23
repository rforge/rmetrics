/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL800b_H
#define WELL800b_H

void InitWELLRNG800b (unsigned int *init);
extern double (*WELLRNG800b)(void);

void GetWELLRNG800b (unsigned int *state);
#endif

