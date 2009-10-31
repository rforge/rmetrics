/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

// This file is entirely based on the source files at
//   http://www.iro.umontreal.ca/~panneton/WELLRNG.html
// The parameters of the generator in this file are replaced by the parameters
// from Tables I and II in the original paper by F. Panneton, P. L'Ecuyer and M. Matsumoto.
// The function GetWELLRNG800a() was add for interface to R package randtoolbox/rngWELL
// by Ch. Dutang and P. Savicky.

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
 *  R-M3-1      case5
 *  R-M3        case4
 *  ...         ...
 *  R-M1-1      case4
 *  R-M1        case3
 *  ...         ...
 *  R-1         case3
 */

#define W 32
#define R 25
#define P 0
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)

#define M1 14
#define M2 18
#define M3 17

#include "WELLmatrices.h"
#include "WELLindexing.h"

#define FACT 2.32830643653869628906e-10

//array with the last R v_i,j's
static unsigned int STATE[R];
static unsigned int z0,z1,z2,y;
static int state_i=0;

static double case_1(void);
static double case_2(void);
static double case_3(void);
static double case_4(void);
static double case_5(void);
static double case_6(void);

double (*WELLRNG800a)(void);

void InitWELLRNG800a(unsigned int *init )
{
  int j;
  state_i=0;
  WELLRNG800a = case_1;
  for(j=0;j<R;j++)
    STATE[j]=init[j];
}

void GetWELLRNG800a (unsigned int *state)
{
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
  z0         = Vrm1Under;
  z1         = MAT1(V0) ^ MAT0NEG(-15,VM1);
  z2         = MAT0POS(10,VM2) ^ MAT0NEG(-11,VM3);
  newV1      = z1 ^ z2;
  newV0Under = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i = R-1;
  WELLRNG800a = case_3;

  return ((double) STATE[state_i] * FACT);
}

// state_i == 1
static double case_2(void)
{
  z0    = Vrm1;
  z1    = MAT1(V0) ^ MAT0NEG(-15,VM1);
  z2    = MAT0POS(10,VM2) ^ MAT0NEG(-11,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i=0;
  WELLRNG800a = case_1;

  return ((double) STATE[state_i] * FACT);
}

// R-1 >= state_i >= R-M1
static double case_3(void)
{
  z0    = Vrm1;
  z1    = MAT1(V0) ^ MAT0NEG(-15,VM1Over);
  z2    = MAT0POS(10,VM2Over) ^ MAT0NEG(-11,VM3Over);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i--;
  if(state_i+M1<R)
    WELLRNG800a = case_4;

  return ((double) STATE[state_i] * FACT);
}

// R-M1-1 >= state_i >= R-M3
static double case_4(void)
{
  z0    = Vrm1;
  z1    = MAT1(V0) ^ MAT0NEG(-15,VM1);
  z2    = MAT0POS(10,VM2Over) ^ MAT0NEG(-11,VM3Over);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i--;
  if (state_i+M3< R)
    WELLRNG800a = case_5;

  return ((double) STATE[state_i] * FACT);
}

// R-M3-1 >= state_i >= R-M2
static double case_5(void)
{
  z0    = Vrm1;
  z1    = MAT1(V0) ^ MAT0NEG(-15,VM1);
  z2    = MAT0POS(10,VM2Over) ^ MAT0NEG(-11,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i--;
  if(state_i+M2 < R)
    WELLRNG800a = case_6;

  return ((double) STATE[state_i] * FACT);
}

// R-M2-1 >= state_i >= 2
static double case_6(void)
{
  z0    = Vrm1;
  z1    = MAT1(V0) ^ MAT0NEG(-15,VM1);
  z2    = MAT0POS(10,VM2) ^ MAT0NEG(-11,VM3);
  newV1 = z1 ^ z2;
  newV0 = MAT0POS(16,z0) ^ MAT3POS(20,z1) ^ MAT1(z2) ^ MAT0NEG(-28,newV1);
  state_i--;
  if(state_i == 1 )
    WELLRNG800a = case_2;

  return ((double) STATE[state_i] * FACT);
}

