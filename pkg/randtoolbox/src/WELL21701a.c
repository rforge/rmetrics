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


/* functions work like this :
 * state_i      function
 *
 *  0               case1
 *  1               case2
 *  2               case6
 *  ...              ...
 *  R-m2-1   case6
 *  R-m2       case5
 *  ...              ...
 *  R-m3-1   case5
 *  R-m3       case4
 *  ...              ...
 *  R-m1-1   case4
 *  R-m1       case3
 *  ...              ...
 *  R-1          case3
 */

#define W 32
#define R 679
#define P 27
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)

#define M1 151
#define M2 327
#define M3 84

//Mi matrices defined in table 1 of Panneton et al (2006)
//matrix M3(t)
#define MAT0POS(t,v) (v^(v>>t))
#define MAT0NEG(t,v) (v^(v<<(-(t))))
//matrix M1
#define MAT1(v) v
//matrix M4(a)
#define MAT2(a,v) ((v & 1U)?((v>>1)^a):(v>>1))
//matrix M2(t)
#define MAT3POS(t,v) (v>>t)
#define MAT3NEG(t,v) (v<<(-(t)))
//matrix M5(t,b)
#define MAT4POS(t,b,v) (v ^ ((v>>  t ) & b))
#define MAT4NEG(t,b,v) (v ^ ((v<<(-(t))) & b))
//matrix M6(q,s,t,a)
#define MAT5(r,a,ds,dt,v) ((v & dt)?((((v<<r)^(v>>(W-r)))&ds)^a):(((v<<r)^(v>>(W-r)))&ds))
//matrix M0
#define MAT7(v) 0

//details of the algorithm figure 1 of Panneton et al. (2006)
// state_i is       i mod R
//v_i,0
#define V0            STATE[state_i]
//v_i,m1, first when i > r-m1
#define VM1Over       STATE[state_i+M1-R]
#define VM1           STATE[state_i+M1]
//v_i,m2, first when i > r-m2
#define VM2Over       STATE[state_i+M2-R]
#define VM2           STATE[state_i+M2]
//v_i,m3, first when i > r-m3
#define VM3Over       STATE[state_i+M3-R]
#define VM3           STATE[state_i+M3]
//v_i,r-1, second when i < R
#define Vrm1          STATE[state_i-1]
#define Vrm1Under     STATE[state_i+R-1]
//v_i,r-2, second when i < R
#define Vrm2          STATE[state_i-2]
#define Vrm2Under     STATE[state_i+R-2]
//v_i+1,0, second when i < R
#define newV0         STATE[state_i-1]
#define newV0Under    STATE[state_i-1+R]
//v_i+1,1
#define newV1         STATE[state_i]
//v_i+1,r-1
#define newVRm1       STATE[state_i-2]
#define newVRm1Under  STATE[state_i-2+R]

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

// state_i == 0
double case_1(void)
{
//        Rprintf("c1 state_i = i mod r : %u\n", state_i);
    
  
  z0 = (Vrm1Under & MASKL) | (Vrm2Under & MASKU);
  z1 = MAT1(V0) ^ MAT0NEG(-26,VM1);
  z2 = MAT0POS(19,VM2) ^ MAT7(VM3);
  newV1  = z1 ^ z2;
  newV0Under = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);

    state_i = R-1;
  WELLRNG21701a = case_3;

       return ((double) STATE[state_i] * FACT);
}

// state_i == 1
static double case_2(void)
{
  //      Rprintf("c2 state_i = i mod r : %u\n", state_i);
    
  
    z0 = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
    z1 = MAT1(V0) ^ MAT0NEG(-26,VM1);
    z2 = MAT0POS(19,VM2) ^ MAT7(VM3);
    newV1  = z1 ^ z2;
    newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);
    
  state_i=0;
  WELLRNG21701a = case_1;

   return ((double) STATE[state_i] * FACT);
}

// state_i+M3 >= R
static double case_3(void)
{
    //    Rprintf("c3 state_i = i mod r : %u\n", state_i);
    
  
    z0 = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
    z1 = MAT1(V0) ^ MAT0NEG(-26,VM1Over);
    z2 = MAT0POS(19,VM2Over) ^ MAT7(VM3Over);
    newV1  = z1 ^ z2;
    newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);
    
  state_i--;
  if(state_i+M3<R)
    WELLRNG21701a = case_4;

   return ((double) STATE[state_i] * FACT);
}

// state_i+M1 >= R
static double case_4(void)
{
    //    Rprintf("c4 state_i = i mod r : %u\n", state_i);
    
  
    z0 = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
    z1 = MAT1(V0) ^ MAT0NEG(-26,VM1Over);
    z2 = MAT0POS(19,VM2) ^ MAT7(VM3Over);
    newV1  = z1 ^ z2;
    newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);
    
  state_i--;
  if (state_i+M1 < R)
    WELLRNG21701a = case_5;

   return ((double) STATE[state_i] * FACT);
}

//state_i+M3 >= R
static double case_5(void)
{
   //     Rprintf("c5 state_i = i mod r : %u\n", state_i);
    
  
    z0 = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
    z1 = MAT1(V0) ^ MAT0NEG(-26,VM1);
    z2 = MAT0POS(19,VM2) ^ MAT7(VM3Over);
    newV1  = z1 ^ z2;
    newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);
    
  state_i--;
  if(state_i+M3 < R)
    WELLRNG21701a = case_6;

   return ((double) STATE[state_i] * FACT);
}

// 2 <= state_i <= R-M3-1
static double case_6(void)
{
   //     Rprintf("c6 state_i = i mod r : %u\n", state_i);
    
    
    z0 = (Vrm1 & MASKL) | (Vrm2Under & MASKU);
    z1 = MAT1(V0) ^ MAT0NEG(-26,VM1);
    z2 = MAT0POS(19,VM2) ^ MAT7(VM3);
    newV1  = z1 ^ z2;
    newV0 = MAT0POS(27,z0) ^ MAT0NEG(-11,z1) ^  MAT5(15,10,27,0x86a9d87e,z2) ^ MAT0NEG(-16,newV1);
    
  state_i--;
  if(state_i == 1 )
    WELLRNG21701a = case_2;

   return ((double) STATE[state_i] * FACT);
}
