/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL44497a_H
#define WELL44497a_H

void InitWELLRNG44497a(unsigned int *);
extern double (*WELLRNG44497a)(void);

void GetWELLRNG44497a (unsigned int *state);
#endif

