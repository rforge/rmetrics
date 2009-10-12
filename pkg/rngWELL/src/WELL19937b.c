/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
/*
 * WELL19937 is __entirely__ based on the code of WELL44497 by P. L'Ecuyer.
 * we just change constants, parameters to get WELL19937, add some
 * code to interface with R and add some comments on #define's.
 */



#define W 32
#define R 624
#define P 31
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)
#define M1 203
#define M2 613
#define M3 123

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
#define VRm1          STATE[state_i-1]
#define VRm1Under     STATE[state_i+R-1]
#define VRm2          STATE[state_i-2]
#define VRm2Under     STATE[state_i+R-2]

#define newV0         STATE[state_i-1]
#define newV0Under    STATE[state_i-1+R]
#define newV1         STATE[state_i]
#define newVRm1       STATE[state_i-2]
#define newVRm1Under  STATE[state_i-2+R]

#define FACT 2.32830643653869628906e-10

static int state_i = 0;
static unsigned int STATE[R];
static unsigned int z0, z1, z2;
static double case_1 (void);
static double case_2 (void);
static double case_3 (void);
static double case_4 (void);
static double case_5 (void);
static double case_6 (void);

double (*WELLRNG19937b) (void);

static unsigned int y;

void InitWELLRNG19937b (unsigned int *init){
   int j;
   state_i = 0;
   WELLRNG19937b = case_1;
   for (j = 0; j < R; j++)
     STATE[j] = init[j];
}

   // state_i == 0
double case_1 (void)
{

   z0 = (VRm1Under & MASKL) | (VRm2Under & MASKU);
   z1 = MAT0POS (7, V0) ^ MAT1 (VM1);
   z2 = MAT0POS (12, VM2) ^ MAT0NEG (-10, VM3);
   newV1      = z1 ^ z2;
   newV0Under = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
  
    state_i = R - 1;
   WELLRNG19937b = case_3;
    
       return ((double) STATE[state_i] * FACT);
}

// state_i == 1
static double case_2 (void)
{

    z0 = (VRm1 & MASKL) | (VRm2Under & MASKU);
    z1 = MAT0POS (7, V0) ^ MAT1 (VM1);
    z2 = MAT0POS (12, VM2) ^ MAT0NEG (-10, VM3);
    newV1      = z1 ^ z2;
    newV0 = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
    
   state_i = 0;
   WELLRNG19937b = case_1;

       return ((double) STATE[state_i] * FACT);
}

// state_i+M3>= R
static double case_3 (void)
{
   
    z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
    z1 = MAT0POS (7, V0) ^ MAT1 (VM1Over);
    z2 = MAT0POS (12, VM2Over) ^ MAT0NEG (-10, VM3Over);
    newV1      = z1 ^ z2;
    newV0 = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
    
   state_i--;
   if (state_i + M3 < R)
      WELLRNG19937b = case_4;

       return ((double) STATE[state_i] * FACT);
}

// state_i+M1 >= R
static double case_4 (void)
{
   
    z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
    z1 = MAT0POS (7, V0) ^ MAT1 (VM1);
    z2 = MAT0POS (12, VM2) ^ MAT0NEG (-10, VM3Over);
    newV1      = z1 ^ z2;
    newV0 = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
    
   state_i--;
   if (state_i + M1 < R)
      WELLRNG19937b = case_5;

       return ((double) STATE[state_i] * FACT);
}

// state_i+M2 >= R
static double case_5 (void)
{
   
    z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
    z1 = MAT0POS (7, V0) ^ MAT1 (VM1);
    z2 = MAT0POS (12, VM2) ^ MAT0NEG (-10, VM3);
    newV1      = z1 ^ z2;
    newV0 = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
    
   state_i--;
   if (state_i + M2 < R)
      WELLRNG19937b = case_6;

       return ((double) STATE[state_i] * FACT);
}

// 2 <= state_i <= (R - M2 - 1)
static double case_6 (void)
{
   
    z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
    z1 = MAT0POS (7, V0) ^ MAT1 (VM1);
    z2 = MAT0POS (12, VM2) ^ MAT0NEG (-10, VM3);
    newV1      = z1 ^ z2;
    newV0 = MAT0NEG (-19,z0) ^ MAT3NEG (-11, z1) ^ MAT0POS (4, z2) ^ MAT0NEG (-10, newV1);
 
    state_i--;
   if (state_i == 1)
      WELLRNG19937b = case_2;

       return ((double) STATE[state_i] * FACT);
}

