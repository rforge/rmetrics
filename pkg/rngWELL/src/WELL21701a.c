/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL21701 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL21701, add some
 * code to interface with R and add some comments on #define's.
 */

/* the assignment of cases is as follows
 * state_i      function
 *
 *  0           case1
 *  1           case2
 *  2           case6
 *  ...         ...
 *  R-M2-1      case6
 *  R-M2        case5
 *  ...         ...
 *  R-M1-1      case5
 *  R-M1        case4
 *  ...         ...
 *  R-M3-1      case4
 *  R-M3        case3
 *  ...         ...
 *  R-1         case3
 */

#define W 32
#define R 679
#define P 27
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)

#define M1 151
#define M2 327
#define M3 84

#include "WELLmatrices.h"
#include "WELLindexing.h"

#define FACT 2.32830643653869628906e-10

static unsigned int STATE[R];
static unsigned int z0,z1,z2,y;
static int state_i=0;

static double case_1(void);
static double case_2(void);
static double case_3(void);
static double case_4(void);
static double case_5(void);
static double case_6(void);

double (*WELLRNG21701a)(void);

void InitWELLRNG21701a(unsigned int *init ){
  int j;
  state_i=0;
  WELLRNG21701a = case_1;
  for(j=0;j<R;j++)
    STATE[j]=init[j];
}

void GetWELLRNG21701a (unsigned int *state){
  int j, k;
  j = 0;
  for (k = state_i; k < R; k++)
    state[j++] = STATE[k];
  for (k = 0; k < state_i; k++)
    state[j++] = STATE[k];
}

// state_i == 0
double case_1(void)
{
  z0         = (Vrm1Under & MASKL) | (Vrm2Under & MASKU);
  z1         = MAT1(V0) ^ MAT0NEG(-26,VM1);
  z2         = MAT0POS(19,VM2) ^ MAT7(VM3);
  newV1      = z1 ^ z2;
  newV0Under = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i = R-1;
  WELLRNG21701a = case_3;

  return ((double) STATE[state_i] * FACT);
}

// state_i == 1
static double case_2(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
  z1    = MAT1(V0) ^ MAT0NEG(-26,VM1);
  z2    = MAT0POS(19,VM2) ^ MAT7(VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i=0;
  WELLRNG21701a = case_1;

  return ((double) STATE[state_i] * FACT);
}

// R-1 >= state_i >= R-M3
static double case_3(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT1(V0) ^ MAT0NEG(-26,VM1Over);
  z2    = MAT0POS(19,VM2Over) ^ MAT7(VM3Over);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i--;
  if(state_i+M3<R)
    WELLRNG21701a = case_4;

  return ((double) STATE[state_i] * FACT);
}

// R_M3-1 >= state_i >= R-M1
static double case_4(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT1(V0) ^ MAT0NEG(-26,VM1Over);
  z2    = MAT0POS(19,VM2Over) ^ MAT7(VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i--;
  if (state_i+M1 < R)
    WELLRNG21701a = case_5;

  return ((double) STATE[state_i] * FACT);
}

// R-M1-1 >= state_i >= R-M2
static double case_5(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT1(V0) ^ MAT0NEG(-26,VM1);
  z2    = MAT0POS(19,VM2Over) ^ MAT7(VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i--;
  if(state_i+M2 < R)
    WELLRNG21701a = case_6;

  return ((double) STATE[state_i] * FACT);
}

// R-M2-1 >= state_i >= 2
static double case_6(void)
{
  z0    = (Vrm1 & MASKL) | (Vrm2 & MASKU);
  z1    = MAT1(V0) ^ MAT0NEG(-26,VM1);
  z2    = MAT0POS(19,VM2) ^ MAT7(VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^ MAT5(15,0x86a9d87e,0xffffffef,0x00200000,z2) ^ MAT0NEG(-16,newV1);
  state_i--;
  if(state_i == 1 )
    WELLRNG21701a = case_2;

  return ((double) STATE[state_i] * FACT);
}

