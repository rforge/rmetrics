/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#ifndef WELL800bTemp_H
#define WELL800bTemp_H

void InitWELLRNG800bTemp (unsigned int *init);
extern double (*WELLRNG800bTemp)(void);

#endif

