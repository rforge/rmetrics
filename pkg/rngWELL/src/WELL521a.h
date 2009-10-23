/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL521a_H
#define WELL521a_H

void InitWELLRNG521a (unsigned int *init);
extern double (*WELLRNG521a)(void);

void GetWELLRNG521a (unsigned int *state);
#endif

