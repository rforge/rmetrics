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

#define W 32
#define R 32
#define M1 22
#define M2 25
#define M3 26

#include "WELLmatrices.h"

#define V0            STATE[state_i                   ]
#define VM1           STATE[(state_i+M1) & 0x0000001fU]
#define VM2           STATE[(state_i+M2) & 0x0000001fU]
#define VM3           STATE[(state_i+M3) & 0x0000001fU]
#define Vrm1          STATE[(state_i+31) & 0x0000001fU]
#define newV0         STATE[(state_i+31) & 0x0000001fU]
#define newV1         STATE[state_i                   ]

#define FACT 2.32830643653869628906e-10

static unsigned int state_i = 0;
static unsigned int STATE[R];
static unsigned int z0, z1, z2;

void InitWELLRNG1024b (unsigned int *init){
  int j;
  state_i = 0;
  for (j = 0; j < R; j++)
    STATE[j] = init[j];
}

void GetWELLRNG1024b (unsigned int *state){
  int j, k;
  j = 0;
  for (k = state_i; k < R; k++)
    state[j++] = STATE[k];
  for (k = 0; k < state_i; k++)
    state[j++] = STATE[k];
}

double WELLRNG1024b (void){
  z0    = Vrm1;
  z1    = MAT0NEG(-21,V0)       ^ MAT0POS (17, VM1);
  z2    = MAT2(0x8bdcb91e, VM2) ^ MAT0POS(15,VM3);
  newV1 = z1                 ^ z2; 
  newV0 = MAT0NEG (-14,z0)   ^ MAT0NEG(-21,z1)    ^ MAT1(z2) ;
  state_i = (state_i + 31) & 0x0000001fU;

  return ((double) STATE[state_i]  * FACT);
}

