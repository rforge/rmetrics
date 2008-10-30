/** 
 * @file  SFMT.c
 * @brief SIMD oriented Fast Mersenne Twister(SFMT)
 *
 * @author Mutsuo Saito (Hiroshima University)
 * @author Makoto Matsumoto (Hiroshima University)
 *
 * Copyright (C) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
 * University. All rights reserved.
 *
 * The new BSD License is applied to this software, 
 *
 *      Redistribution and use in source and binary forms, with or without
 *      modification, are permitted provided that the following conditions are
 *      met:
 *      
 *          - Redistributions of source code must retain the above copyright
 *          notice, this list of conditions and the following disclaimer.
 *          - Redistributions in binary form must reproduce the above
 *          copyright notice, this list of conditions and the following
 *          disclaimer in the documentation and/or other materials provided
 *          with the distribution.
 *          - Neither the name of the Hiroshima University nor the names of
 *          its contributors may be used to endorse or promote products
 *          derived from this software without specific prior written
 *          permission.
 *     
 *      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *  
 */
#include <string.h>
#include <assert.h>

#include "SFMT.h"
/* no longer needed #include "SFMT-params.h" */




#if defined(__BIG_ENDIAN__) && !defined(__amd64) && !defined(BIG_ENDIAN64)
#define BIG_ENDIAN64 1
#endif
#if defined(HAVE_ALTIVEC) && !defined(BIG_ENDIAN64)
#define BIG_ENDIAN64 1
#endif
#if defined(ONLY64) && !defined(BIG_ENDIAN64)
  #if defined(__GNUC__)
    #error "-DONLY64 must be specified with -DBIG_ENDIAN64"
  #endif
#undef ONLY64
#endif
/*------------------------------------------------------
  128-bit SIMD data type for Altivec, SSE2 or standard C
  ------------------------------------------------------*/
#if defined(HAVE_ALTIVEC)
  #if !defined(__APPLE__)
    #include <altivec.h>
  #endif
/** 128-bit data structure */
union W128_T {
    vector unsigned int s;
    uint32_t u[4];
};
/** 128-bit data type */
typedef union W128_T w128_t;

#elif defined(HAVE_SSE2)
  #include <emmintrin.h>

/** 128-bit data structure */
union W128_T {
    __m128i si;
    uint32_t u[4];
};
/** 128-bit data type */
typedef union W128_T w128_t;

#else

/** 128-bit data structure */
struct W128_T {
    uint32_t u[4];
};
/** 128-bit data type */
typedef struct W128_T w128_t;

#endif

/*--------------------------------------
  FILE GLOBAL VARIABLES
  internal state, index counter and flag 
  --------------------------------------*/
/** the 128-bit internal state array
static w128_t sfmt[N]; */
/** the 32bit integer pointer to the 128-bit internal state array
static uint32_t *psfmt32 = &sfmt[0].u[0];
#if !defined(BIG_ENDIAN64) || defined(ONLY64) */
/** the 64bit integer pointer to the 128-bit internal state array
static uint64_t *psfmt64 = (uint64_t *)&sfmt[0].u[0];
#endif */
 /** index counter to the 32-bit internal state array
static int idx; */
/** a flag: it is 0 if and only if the internal state is not yet
 * initialized. 
static int initialized = 0; */
/** a parity check vector which certificate the period of 2^{MEXP} 
static uint32_t parity[4] = {PARITY1, PARITY2, PARITY3, PARITY4}; */




/*
 * code of Christophe Dutang 
 * added to interface with R 
 */
/* ===================  my code  =================== */

/* global variables such as sfmt, psfmt32, psfmt64 are defined below */

/* global constants */ 

//number of paramter sets
#define NBSET 32

// D param, not used for the moment
const int SFMT607_D_ParamSet[NBSET] = {
96,     96,     98,     102, 119,    134,    135,    136,
138,    155,    155,    159, 159,    159,    160,    169,
175,    176,    181,    195, 196,    197,    204,    207,
212,    213,    214,    215, 220,    221,    226,    239};

const int SFMT1279_D_ParamSet[NBSET] = {
    257,    258,    268,    279,    305,    309,    312,    338,
    351,    354,    362,    364,    372,    379,    386,    388,
    400,    401,    405,    408,    410,    414,    415,    445,
    464,    469,    481,    481,    493,    493,    497,    499};

const int SFMT2281_D_ParamSet[NBSET] = {
    416,    428,    489,    496,    505,    539,    564,    568,
    621,    624,    658,    662,    666,    707,    740,    752,
    762,    764,    764,    811,    888,    891,    901,    967,
    979,    1010,    1044,    1078,    1116,    1118,    1121,    1122};

const int SFMT4253_D_ParamSet[NBSET] = {
    861,    929,    1031,    1136,    1165,    1209,    1245,    1266,
    1314,    1316,    1344,    1394,    1496,    1552,    1558,    1571,
    1615,    1628,    1640,    1659,    1674,    1714,    1744,    1814,
    1815,    1843,    1851,    1876,    1890,    1970,    1979,    2030};    

const int SFMT11213_D_ParamSet[NBSET] = {
    2264,    2337,    2366,    2411,    2581,    2665,    2742,    2940,
    2946,    3024,    3205,    3258,    3343,    3357,    3451,    3512,
    3543,    3654,    3726,    3833,    3915,    4030,    4260,    4366,
4371,    4390,    4477,    4587,    4709,    4764,    4776,    4874};
    
const int SFMT19937_D_ParamSet[NBSET] = {
    4188,    3281,    3316,    3352,    3829,    3909,    3935,    3948,
    4059,    4151,    4203,    4242,    4264,    4302,    4324,    4393,
    4434,    4595,    4728,    4936,    4941,    5089,    5239,    5390,
5612,    5658,    5802,    5849,    5864,    5929,    5943,    5951};
    
    
    

// POS1 param
const int SFMT607_POS1_ParamSet[NBSET] = {
2,      2,      1,      3,      2,      1,      1,      2,      1,      
1,      3,      1,      1,      3,      1,      2,      1,      1,      
1,      2,      1,      2,      1,      3,      2,      1,      3,      
2,      2,      3,      1,      1};

const int SFMT1279_POS1_ParamSet[NBSET] = {
    3,    7,    6,    8,    3,    5,    6,    6,
    2,    8,    1,    1,    8,    3,    1,    8,
    6,    2,    7,    8,    6,    7,    4,    7,
    6,    8,    2,    6,    6,    8,    6,    3};

const int SFMT2281_POS1_ParamSet[NBSET] = {
    12,    12,    7,    8,    5,    10,    11,    3,
    11,    2,    5,    15,    5,    16,    15,    9,
    4,    1,    6,    1,    1,    11,    10,    11,
9,    5,    3,    14,    3,    2,    15,    9};

const int SFMT4253_POS1_ParamSet[NBSET] = {
    17,    25,    15,    28,    10,    8,    13,    14,
    23,    16,    28,    12,    13,    18,    11,    1,
    16,    13,    4,    32,    9,    32,    32,    13,
    27,    19,    12,    25,    24,    4,    21,    31};

const int SFMT11213_POS1_ParamSet[NBSET] = {
    68,    73,    72,    63,    73,    48,    45,    58,
    86,    84,    33,    22,    79,    77,    45,    41,
    69,    48,    9,    48,    36,    12,    37,    85,
42,    38,    80,    37,    14,    16,    85,    13};

const int SFMT19937_POS1_ParamSet[NBSET] = {
    122,    73,    60,    45,    135,    120,    33,    122,
    64,    29,    128,    106,    75,    85,    27,    12,
    92,    124,    112,    130,    125,    4,    123,    97,
2,    153,    64,    150,    150,    110,    22,    104};

// SL1 param
const int SFMT607_SL1_ParamSet[NBSET] = {
    15,     19,     13,    11,    15,       7,
    14,    14,    14,    5,    13,    12,    13,
    6,    23,    13,    17,    7,    5,    10,
    4,    5,    3,    11,    21,    13,    11,
11,    21,    20,    18,    21};

const int SFMT1279_SL1_ParamSet[NBSET] = {
    18,    14,    21,    19,    20,    15,    13,    17,
    18,    21,    25,    6,    6,    13,    19,    6,
    5,    19,    13,    6,    13,    27,    26,    11,
    5,    13,    12,    3,    19,    11,    14,    5};

const int SFMT2281_SL1_ParamSet[NBSET] = {
    19,    10,    9,    10,    10,    10,    8,    14,
    14,    20,    10,    13,    5,    18,    19,    10,
    14,    11,    9,    9,    18,    19,    19,    17,
21,    4,    2,    20,    2,    9,    12,    11};
    
const int SFMT4253_SL1_ParamSet[NBSET] = {
    20,    10,    25,    8,    17,    7,    18,    6,    
    11,    16,    5,    17,    13,    13,    21,    12,    
    18,    13,    11,    3,    5,    3,    21,    22,    
    11,    26,    29,    22,   2,    14,    20,    15};    

const int SFMT11213_SL1_ParamSet[NBSET] = {
    14,    11,    13,    22,    12,    10,    13,    7,
    19,    22,    6,    14,    22,    9,    17,    13,
    6,    12,    18,    11,    5,    7,    21,    19,
13,    16,    3,    21,    11,    27,    5,    15};

const int SFMT19937_SL1_ParamSet[NBSET] = {
    18,    11,    10,    14,    18,    18,    14,    12,
    10,    21,    11,    19,    9,    10,    21,    13,
    9,    13,    20,    9,    11,    20,    16,    14,
13,    11,    7,    7,    16,    17,    5,    9};

    
    
// SL2 param
const int SFMT607_SL2_ParamSet[NBSET] = {
    3,    1,    3,    3,    1,    3,    1,    1,
    5,    3,    1,    7,    7,    7,    1,    1,
    3,    3,    3,    3,    3,    3,    3,    5,
5,    3,    5,    5,    3,    7,    1,    5};

const int SFMT1279_SL2_ParamSet[NBSET] = {
    1,    3,    1,    1,    1,    3,    3,    3,
    1,    1,    1,    3,    3,    3,    3,    3,
    3,    3,    7,    7,    1,    1,    1,    3,
    7,    1,    5,    3,    1,    1,    1,    3};

const int SFMT2281_SL2_ParamSet[NBSET] = {
    1,    3,    3,    3,    3,    3,    3,    3,
    7,    1,    3,    1,    3,    5,    3,    7,
    3,    3,    7,    3,    5,    7,    3,    7,
3,    7,    7,    7,    3,    7,    1,    5};

const int SFMT4253_SL2_ParamSet[NBSET] = {
    1,    3,    1,    3,    3,    3,    1,    3,
    7,    3,    3,    5,    5,    3,    3,    1,
    5,    5,    1,    3,    3,    3,    3,    1,
    5,    1,    1,    5,    3,    5,    1,    5};    

const int SFMT11213_SL2_ParamSet[NBSET] = {
    3,    3,    3,    1,    3,    3,    3,    7,
    1,    1,    7,    7,    1,    3,    1,    1,
    3,    3,    5,    7,    7,    3,    3,    7,
7,    3,    3,    7,    1,    1,    7,    5};

const int SFMT19937_SL2_ParamSet[NBSET] = {
    1,    3,    3,    3,    1,    1,    3,    3,
    3,    1,    3,    1,    3,    3,    1,    3,
    3,    3,    1,    3,    3,    1,    3,    1,
3,    3,    7,    3,    3,    3,    3,    7};

    
    
// SR1 param
const int SFMT607_SR1_ParamSet[NBSET] = {
    13,    13,    11,    21,    3,    2,    1,
    11,    3,    6,    3,    9,    5,    1,    20,
    16,    22,    21,    15,    23,    1,    18,
    2,    4,    4,    18,    12,    6,    11,
7,    7,    19};

const int SFMT1279_SR1_ParamSet[NBSET] = {
    1,    5,    17,    15,    5,    12,    21,    5,
    11,    3,    6,    13,    9,    12,    4,    3,
    3,    13,    2,    5,    1,    2,    3,    12,
    11,    5,    3,    5,    7,    5,    3,    22};

const int SFMT2281_SR1_ParamSet[NBSET] = {
    5,    5,    4,    7,    9,    9,    5,    11,
    5,    5,    15,    4,    6,    1,    14,    13,
    13,    24,    21,    12,    7,    12,    17,    20,
20,    19,    1,    9,    19,    17,    1,    6};

const int SFMT4253_SR1_ParamSet[NBSET] = {
    7,    1,    2,    3,    13,    19,    21,    3,
    6,    11,    11,    5,    12,    17,    6,    7,
    7,    4,    3,    11,    26,    3,    7,    9,
    8,    19,    1,    19,    7,    9,    19,    14};    

const int SFMT11213_SR1_ParamSet[NBSET] = {
    7,    2,    1,    13,    9,    9,    10,    5,
    5,    7,    7,    7,    17,    10,    6,    4,
    3,    17,    15,    17,    17,    11,    4,    10,
21,    1,    1,    6,    21,    5,    14,    10};

const int SFMT19937_SR1_ParamSet[NBSET] = {
    11,    4,    3,    5,    1,    1,    5,    3,
    13,    2,    1,    9,    4,    5,    12,    3,
    5,    11,    15,    13,    16,    1,    3,    11,    
11,    4,    7,    3,    3,    9,    7,    13};
    

// SR2 param
const int SFMT607_SR2_ParamSet[NBSET] = {
    3,    1,    5,   1,    1,    1,    1,    1,
    1,    3,    3,    1,    1,    1,    1,    3,
    5,    3,    5,    1,    1,    5,    3,    1,
1,    7,    3,    3,    5,    1,    1,    1};

const int SFMT1279_SR2_ParamSet[NBSET] = {
    1,    1,    1,    1,    1,    1,    1,    3,
    1,    7,    1,    3,    1,    5,    1,    5,
    3,    3,    1,    1,    7,    1,    1,    5,
    1,    3,    3,    3,    3,    3,    7,    5};

const int SFMT2281_SR2_ParamSet[NBSET] = {
    1,    1,    1,    1,    5,    3,    5,    5,
    1,    3,    5,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    3,    3,    1,    1,
5,    1,    1,    1,    1,    1,    7,    3};

const int SFMT4253_SR2_ParamSet[NBSET] = {
    1,    3,    1,    5,    3,    1,    1,    5,
    1,    3,    5,    3,    3,    7,    1,    1,
    3,    3,    3,    3,    1,    5,    1,    3,
    3,    7,    1,    3,    1,    3,    3,    3};    

const int SFMT11213_SR2_ParamSet[NBSET] = {
    3,    5,    1,    1,    1,    5,    5,    1,
    1,    1,    1,    1,    1,    3,    3,    3,
    1,    1,    1,    1,    1,    1,    5,    1,
1,    5,    3,    1,    1,    3,    1,    3};

const int SFMT19937_SR2_ParamSet[NBSET] = {
    1,    1,    1,    5,    1,    1,    1,    5,
    1,    1,    5,    1,    3,    5,    1,    1,
    3,    1,    1,    1,    3,    1,    1,    1,
1,    3,    1,    1,    1,    3,    1,    1};

    
    
// MSK1 param
const long int SFMT607_MSK1_ParamSet[NBSET] = {
    0xfdff37ffU,    0xffbfffbfU,    0xdf7fffffU,    0x6fff7ffeU,
    0xff77fff7U,    0xbedf9cffU,    0xffefffffU,    0x7bfffbffU,
    0xffe7ffffU,    0xd3ddebfbU,    0x7fffd6beU,    0xfeffffdeU,
    0xfffff9b7U,    0xfeffbff7U,    0xf5ffbfffU,    0xfffffbbfU,
    0xdffffafeU,    0xbffddfefU,    0xffffdf7fU,    0xffebccfdU,
    0xfe9fffefU,    0xfcbeffcfU,    0xffffffffU,    0x7fdfffffU,
    0xffbbf77fU,    0xdfbf5fffU,    0xebffefffU,    0xffffffddU,
0xffff7ebfU,    0xbeffbffbU,    0xf7ffb5fdU,    0xbfff7effU};

const long int SFMT1279_MSK1_ParamSet[NBSET] = {
    0xfffffffbU,    0xf7fefffdU,    0xfffbbfffU,    0xfbefeffdU,
    0xfddfbf9fU,    0xfddf9f97U,    0xfffbffffU,    0xafffffffU,
    0xdeffffefU,    0xfffdfdfeU,    0xeff6fdedU,    0xffeecff3U,
    0xeffff777U,    0xffdf7ff7U,    0xff7fffffU,    0xf7f9f7ddU,
    0xdfefeffdU,    0xfff97fffU,    0x7df7ffffU,    0xdf7ffaffU,
    0xfddbffffU,    0xfffb67ffU,    0xfffdfffaU,    0xfdfeedffU,
    0x4fdffdffU,    0xffdffbaeU,    0xbfdefeefU,    0xfffffff3U,
    0xfdffefffU,    0x7eafebffU,    0xffffbffdU,    0xcffdffdfU};

const long int SFMT2281_MSK1_ParamSet[NBSET] = {
    0xbff7ffbfU,    0xfbdffff7U,    0xdfffef7bU,    0xdbffedffU,
    0x3f7ffbffU,    0xff9ffff7U,    0xff6ffba9U,    0x7bffffdfU,
    0xf7bffff9U,    0xffffffbfU,    0xfff3ecfdU,    0xffffff1bU,
    0xdfff7dffU,    0x7ffffbffU,    0xffffdbbfU,    0xdfffeeefU,
    0xd7ffffdfU,    0xf3fbbfffU,    0xbeddbdffU,    0xeff7fcfaU,
    0xefffffe7U,    0xf7bfffffU,    0xeffbdfffU,    0xbdffddfdU,
    0xffffffdfU,    0xffbfbfffU,    0xfffefbefU,    0x7bffc3f9U,
0xbbfffcffU,    0xffffffffU,    0xfdffffffU,    0xe7fffba3U};

const long int SFMT4253_MSK1_ParamSet[NBSET] = {
    0x9f7bffffU,    0xdaeeedf7U,    0xdffff3ffU,    0xafffdffbU,
    0xbffdefffU,    0xbffffffbU,    0xe777f7fcU,    0x7fdff3fbU,
    0xffbfbe7fU,    0xfdffff4fU,    0xf8fefff7U,    0xffffffffU,
    0x3ff7ff7fU,    0xbdf7ffffU,    0xfffffea7U,    0xffbfff7bU,
    0xdfbfffffU,    0xbf7febadU,    0xf7efdbfbU,    0xffffeffdU,
    0xb9ffffdfU,    0xfffffeffU,    0xff3feefdU,    0xffffebfbU,
    0xd73d23f7U,    0xefbfbff7U,    0xfff7dffdU,    0xefffffefU,
    0xef8ffffaU,    0x7bfffff7U,    0xffb7fffdU,    0xefbce7ffU};

const long int SFMT11213_MSK1_ParamSet[NBSET] = {
    0xeffff7fbU,    0xff6ffcffU,    0x7ff7ffffU,    0xf5fbfffbU,
    0xddffdff7U,    0x5dffefcdU,    0xfbffd7ffU,    0x7defffffU,
    0xfffffebfU,    0xfff7effeU,    0xfffff9ffU,    0xfffefbf7U,
    0xffbf7ff7U,    0xfffffffeU,    0xf77bbfefU,    0xfb5d7fffU,
    0xeffdfdffU,    0xde5fffbfU,    0x5fe7eff7U,    0xffd7ebffU,
    0xccfe7ffeU,    0xfffbe5fbU,    0x76fe7fffU,    0xfffdfbffU,
    0xff3ebfbeU,    0xfdf7dbffU,    0xf77efedfU,    0xffcffdffU,
0xffffffbdU,    0xdfffef7fU,    0xffbfeffeU,    0xfffaffffU};
    
const long int SFMT19937_MSK1_ParamSet[NBSET] = {
    0xdfffffefU,    0xefefffffU,    0xeffdbfffU,    0xaefefbbdU,
    0xffffdffeU,    0xffefef5dU,    0xfdff7ffdU,    0xfef73bffU,
    0xfff8bfffU,    0x5fffffdfU,    0x7dbebfffU,    0xffeeffdfU,
    0xfffeffffU,    0xbdfffbefU,    0xbf7dffdfU,    0xffffffbbU,
    0xcfffbbffU,    0xffbff7efU,    0xffbf7efeU,    0xdfbf1ffdU,
    0xbfdfffffU,    0xfffef7f9U,    0xbd7feffdU,    0xefdfceffU,
    0xfdffbffdU,    0xffebf7fdU,    0xfbbefeefU,    0xffff7fefU,
0xfdbffaedU,    0xbffffffbU,    0xbb5fffffU,    0xbfefefebU};

    

// MSK2 param
const long int SFMT607_MSK2_ParamSet[NBSET] = {
    0xef7f3f7dU,    0xcdffffffU,    0xf7ff77ffU,    0xf5fffffeU,
    0xfd77ffffU,    0xdfe6dfffU,    0x7e6ffa77U,    0xfba7ffbfU,
    0xff6fefffU,    0xffdfffdfU,    0xffbefdfcU,    0xfe8fbfb7U,
    0xbfebffdbU,    0xfffdd7ffU,    0xbffcfffeU,    0xfffdffefU,
    0xdffffeffU,    0xffffffd7U,    0xbfffed97U,    0xaffffffdU,
    0xffbffeffU,    0x5bfebb7fU,    0xffffeef7U,    0x7cddd7faU,
    0xff7ffbfbU,    0xcffd7effU,    0xfbfffaffU,    0xdff7bdf7U,
0xfffd7fffU,    0xbffffffbU,    0xb76ff7fdU,    0x7fffbfefU};

const long int SFMT1279_MSK2_ParamSet[NBSET] = {
    0x37f5effbU,    0x7fefcfffU,    0xfffbdffbU,    0xffefffffU,
    0xeefffb37U,    0xf97fbff7U,    0xfffe96efU,    0xff37bffbU,
    0xdeb7effbU,    0xfbbffeffU,    0xfffffdfcU,    0xffffedffU,
    0xeefbfdffU,    0xfff4ffefU,    0xfffbfd7fU,    0xb7f7beffU,
    0xdfffffbfU,    0x6bfff7feU,    0xdfd7ff7fU,    0xdffaffffU,
    0xebffdffcU,    0xdefbe67bU,    0xffffbfebU,    0xdffcfff7U,
    0xc7feff7fU,    0x7ffbff3fU,    0xbffffeffU,    0xf7bbffffU,
    0x5ffbdffdU,    0x7fdfdefdU,    0xfffbffdfU,    0xffff6effU};

const long int SFMT2281_MSK2_ParamSet[NBSET] = {
    0xfdfffffeU,    0xcfbffffeU,    0xffefffffU,    0xfffbfffdU,
    0xfffffff9U,    0xfdffffffU,    0xfffff7f7U,    0x3ffb7fdbU,
    0x6efff7ffU,    0x7dffdfebU,    0xffffff77U,    0xffbff77bU,
    0xff79fffeU,    0xfffff7bfU,    0xcbf7ffffU,    0xfffffddfU,
    0xbfdfffffU,    0xbffffffbU,    0xfbffbd7fU,    0xf3ee7fefU,
    0xffffdfdfU,    0xeddfffffU,    0xfffdbeedU,    0xdfdffffcU,
    0xdfffffffU,    0xffffbfffU,    0x7ffffbf7U,    0xddfff2fbU,
0xf7bffcdfU,    0x7fbff9dfU,    0xffffdef7U,    0xffdf7fdfU};

const long int SFMT4253_MSK2_ParamSet[NBSET] = {
    0x9fffff5fU,    0xfff4eff7U,    0xffdebf7fU,    0xffffdfbfU,
    0xffaffeffU,    0x6f9ffbffU,    0xf7d7bf7dU,    0x57dfffdbU,
    0xffbfdf6dU,    0xfff7fbffU,    0xdfffffffU,    0xbdffffffU,
    0xffffdfdfU,    0xfbdfbfffU,    0xfffdff3aU,    0xfabf7ffbU,
    0xfb7e5fffU,    0xff3ff756U,    0xf7fffffbU,    0xffffeff7U,
    0xf9fdff7fU,    0xfffb9d7fU,    0xffffecffU,    0x7fffbb7fU,
    0xef7fffffU,    0xfb7fffffU,    0xffdd7fbdU,    0xfbdfb3ffU,
    0xfffbfff8U,    0xfff7fff3U,    0x9fefedfdU,    0xfeb7f77fU};

const long int SFMT11213_MSK2_ParamSet[NBSET] = {
    0xffffffefU,    0xbff77dffU,    0xafffcddeU,    0xffffffbfU,
    0x6baffff7U,    0xc6ffffdfU,    0xddf7f7e7U,    0x7d9bffffU,
    0xfdeffadfU,    0xbfb7afffU,    0xfb7fffffU,    0xeffffb7eU,
    0x9dffdfdfU,    0x3feefbffU,    0xbcf7bfdfU,    0xfbff7bffU,
    0xf9bffbffU,    0xf7d5f7beU,    0xdbffdff3U,    0xefffff7bU,
    0xfadfffffU,    0xfdfdf7dfU,    0x79ffffffU,    0xffbffa3fU,
    0xff7fffdeU,    0xfbffdffbU,    0xf7fffbebU,    0xfedffdffU,
0xff7feffeU,    0xeff97fdfU,    0xfbfddbebU,    0xeffef9ffU};

const long int SFMT19937_MSK2_ParamSet[NBSET] = {
    0xddfecb7fU,    0xef7dfffbU,    0xdfffffffU,    0xfeffffffU,
    0xfffefffbU,    0xff25ff7dU,    0xfbffee71U,    0x65eb7ff7U,
    0xffffffffU,    0xffffcfffU,    0x7ffffffeU,    0xffffdfffU,
    0xffeff7dfU,    0xefedffefU,    0xc97dff7bU,    0xfffff7abU,
    0xfffffff7U,    0x7b7e7fcdU,    0xfabf7fffU,    0xf7feefddU,
    0x7fffbe7fU,    0xffdff7fbU,    0xbfffffffU,    0xeddffdffU,
    0xfff3b33fU,    0xfffdffffU,    0xffffffc7U,    0xffbf5affU,
0xffc7f69fU,    0x7b7fffffU,    0xbe7fdfffU,    0x77f7efbfU};



// MSK3 param
const long int SFMT607_MSK3_ParamSet[NBSET] = {
    0xff777b7dU,    0xeffbffedU,    0xfffff2ffU,    0xbffffffeU,
    0xfffffdcfU,    0xfdfffffaU,    0xfff7fffaU,    0xf7fffe3fU,
    0x7f3fffffU,    0xffbf9bffU,    0xf7ff73f5U,    0x63effffdU,
    0xe7dfeefeU,    0x7ffffdffU,    0xffbffefeU,    0xefb97fffU,
    0xfd77ffdfU,    0x3ffffff7U,    0xffeadcbfU,    0xf777bdffU,
    0xbe57bdffU,    0xfff5efffU,    0xbff6ff73U,    0xfebfdeeeU,
    0xbffff7f7U,    0xffbe3fffU,    0xf7fffeffU,    0xf3fff1ffU,
0xdffdfdffU,    0xffffefffU,    0xfdfff7dfU,    0xfffbff7fU};

const long int SFMT1279_MSK3_ParamSet[NBSET] = {
    0xb3fdaff9U,    0xaff3ef3fU,    0xddfffdffU,    0xdfef7effU,
    0xfff7df7bU,    0x7fbfbff7U,    0xedd7ff2fU,    0xe7bdffddU,
    0xdf77defeU,    0xef6fffffU,    0xffffbfffU,    0xdbf7bfffU,
    0xfeffffffU,    0xedfe5fefU,    0xffefffeaU,    0xf7ffdf7fU,
    0xdeffb9f7U,    0xdafffb7dU,    0xf7f2b3ddU,    0xfdd5bfefU,
    0xffeff7fdU,    0xef1fffffU,    0xfffdff9aU,    0x7bdffffeU,
    0xf5ffbe7fU,    0xffffab5bU,    0xfffefffbU,    0xfafff9fbU,
    0xefbbfff3U,    0xb7d7bff5U,    0xffffffffU,    0xfff5fff7U};

const long int SFMT2281_MSK3_ParamSet[NBSET] = {
    
    0xf7ffef7fU,    0xfdfffffbU,    0xfcfeffffU,    0xf7ffffdfU,
    0xbffffbfeU,    0xbf3f7fffU,    0xffdfdfffU,    0x3f777ffbU,
    0xfbfbffdfU,    0xdf7fffdfU,    0x9f9fadffU,    0xbfbffffeU,
    0xeff77ffdU,    0xdf7e6fffU,    0xfffebbfdU,    0xfbfbfe5fU,
    0xfbfffdfeU,    0xdf7fd7bdU,    0xf95b7fffU,    0xbdb47dffU,
    0xffffffffU,    0xf77ffd7fU,    0xfefd3fbfU,    0xbffeffd6U,
    0xff6ffffaU,    0xfff7ceeeU,    0xffffffffU,    0xddfbf6fbU,
0xe39ffffdU,    0x7f6fefcfU,    0xff3fffffU,    0xefbf7fffU};

const long int SFMT4253_MSK3_ParamSet[NBSET] = {
    0x3efffffbU,    0xffffebfeU,    0xffdfbdf7U,    0xfbffdf77U,
    0x9f3ebfceU,    0x7f9fe7f7U,    0xedff7f5fU,    0xfe9ffddfU,
    0xbfbffdfdU,    0xee7ffdffU,    0x7fffffffU,    0xe7fffffeU,
    0xffbdbf7fU,    0x7fdffb3fU,    0xbfffffcfU,    0xebfffefdU,
    0xfffbdfffU,    0xfafaf6f6U,    0xfffefe7fU,    0xffffffefU,
    0xfffebffeU,    0xffdb9ff7U,    0xfffffcfdU,    0xcbddfffbU,
    0xeeffffdfU,    0xfeeffdfbU,    0xffffbff7U,    0xfbf7f7fdU,
    0xf7ffbfe9U,    0xff9fbf9eU,    0x9edb6ffbU,    0xfff3ffbfU};

const long int SFMT11213_MSK3_ParamSet[NBSET] = {
    0xdfdfbfffU,    0x77dfffffU,    0xeffffeffU,    0xdf7ffcb7U,
    0x7bd5ffffU,    0xfe77bfffU,    0x9bffe71fU,    0xfdbff7eeU,
    0x7fb71fcfU,    0xb7dfffffU,    0xebeffdfeU,    0xfefdfbbfU,
    0xdfef97bfU,    0xf7fefeffU,    0xbffff7e7U,    0xf7ff677fU,
    0xdff1ffffU,    0xfff7757dU,    0xdbff6ff7U,    0xefbfffbdU,
    0xfffffdffU,    0xfffdf7ffU,    0x7ff7dfffU,    0xffbffa2fU,
    0x5ffffbefU,    0xfbffbfd9U,    0xcbffffe7U,    0xfcbffffbU,
0xb6ffd7ffU,    0xfff3fcfaU,    0xfbdfffffU,    0xfe7e9bdbU};

const long int SFMT19937_MSK3_ParamSet[NBSET] = {
    0xbffaffffU,    0xbcfffeffU,    0xefffbfefU,    0xd7edffffU,
    0xbfbfffffU,    0xeebfefeeU,    0xfb3ff7dfU,    0x76ffffffU,
    0x5fffffbfU,    0xbdffdefeU,    0xded6ffefU,    0xbd6ff9ffU,
    0xbbbffeefU,    0xaff9f7feU,    0xf7fdfbfdU,    0xfdfff7c2U,
    0xfff7ffefU,    0xfddaff7fU,    0xf6ff7fefU,    0xfffbffffU,
    0xbbfffff7U,    0xcbfff7feU,    0xfffcf6ffU,    0xefdfef7fU,
    0xfff7ff7eU,    0xffffffefU,    0xfbffed57U,    0xa6ff7fbdU,
0x67f7ffbeU,    0xffffdf5fU,    0xfe7fffdfU,    0xf5ffff7eU};

    
    
// MSK4 param
const long int SFMT607_MSK4_ParamSet[NBSET] = {
    0x7ff7fb2fU,    0xfef8fbdcU,    0xffff473fU,    0xafdfffffU,
    0xfffffffaU,    0xdfff7efbU,    0xfbbffffbU,    0xf36fffffU,
    0xff7feffbU,    0xefefbfffU,    0x37fff7fbU,    0x7fe7b3f5U,
    0xf3ff3effU,    0xfdffffffU,    0x7fffbeefU,    0xdf7fffefU,
    0xfbfffffbU,    0xfff5effbU,    0xf7ffbefbU,    0xaf7effffU,
    0xfefb3f5fU,    0xefb5eff9U,    0xffe7f7d7U,    0xffbffef7U,
    0xefecdfffU,    0xfdbfff9fU,    0x7ffefffeU,    0xffff7ffeU,
0xffa57bdfU,    0xeefbef3fU,    0xf9dffffeU,    0xeffeff6fU};

const long int SFMT1279_MSK4_ParamSet[NBSET] = {
    0xfffffd7dU,    0xb5ffff7fU,    0x57ffffbfU,    0xefeeffffU,
    0xfffefbefU,    0xbfbf1fffU,    0xf5f7f8edU,    0xfffdffddU,
    0xdfdfd5cfU,    0xff7f76efU,    0xcfefffffU,    0xd1ffdffdU,
    0xfff7fffeU,    0xff7fdfffU,    0xfddfdfdeU,    0xffdff7f9U,
    0xfffffef7U,    0xefffffffU,    0xe77ffddfU,    0xffd7bfddU,
    0xdfdffeffU,    0x3effff8fU,    0xbfffffdfU,    0xffdfffeaU,
    0x3fddff97U,    0xe7fff35bU,    0xfbfbdffeU,    0xff77fddfU,
    0xff7e7bb7U,    0xf7bb7ff7U,    0xfedffde5U,    0xefffbfffU};

const long int SFMT2281_MSK4_ParamSet[NBSET] = {
    0xf2f7cbbfU,    0xbcdffffbU,    0xb6ff77cfU,    0xffff7ff7U,
    0x9ffff7ffU,    0xfbffff7cU,    0xf2ffffffU,    0xfffefdbbU,
    0xfff8ffbfU,    0xfeff69eeU,    0xb9d3ebddU,    0xfffbfaf5U,
    0xeffb75dfU,    0xfff7cfcfU,    0xbefebfffU,    0xcffbfdefU,
    0x5ffbffffU,    0xff7edffdU,    0x7fefbfffU,    0xdbf6dfffU,
    0x5fff4affU,    0xeff7ffffU,    0xff5beffeU,    0xff7fffffU,
    0xffffdfffU,    0xffffefffU,    0x6fff7fffU,    0x77dfdfffU,
0xf7bebfffU,    0xefffffdeU,    0xf37776f7U,    0xfd7cf7bfU};

const long int SFMT4253_MSK4_ParamSet[NBSET] = {
    0xfffff7bbU,    0xeff7febfU,    0xfbffffffU,    0x7d7cfef7U,
    0xbf9fffdfU,    0xbfeffffbU,    0xffbfff7fU,    0xfefdae7fU,
    0xfffffffdU,    0xdfbbfdffU,    0xfffdfbf7U,    0xf3b7f7efU,
    0xffffbeffU,    0xffbe6befU,    0xdff5ffedU,    0x55fdfffeU,
    0xdffbbfffU,    0xfffffffcU,    0xf7eb7fefU,    0xffdfffffU,
    0xb6ff7ffdU,    0xbff9fff7U,    0xfffbadfdU,    0xfff7ff7fU,
    0xfffbf6cfU,    0x7fbfebf9U,    0xffff9dffU,    0xfef7fffbU,
    0x77ffbffdU,    0xfffdafffU,    0xbedeb7ffU,    0xbfffb75fU};

const long int SFMT11213_MSK4_ParamSet[NBSET] = {
    0x7fffdbfdU,    0xfbfffffbU,    0xffffffffU,    0xdffff7ffU,
    0xefd3fbffU,    0xffefefffU,    0xff7ec3ffU,    0xdfffffffU,
    0xffff5fffU,    0xbf7f7fefU,    0xfbfbffffU,    0xe7ffffdfU,
    0xdfeebf7fU,    0xf5bc9fffU,    0xfffaff7bU,    0xc67fffbfU,
    0xf7f8dfffU,    0xfffeef79U,    0xbb377fffU,    0xf7f7efaeU,
    0xffefefffU,    0xfffffffbU,    0xbbfbefffU,    0xffdfffbfU,
    0xfffefbfbU,    0xfffffdbdU,    0xfdfffbffU,    0xffeffebfU,
0x97f9fe7fU,    0xfe7feeffU,    0xbfffffffU,    0xff7ffbfeU};

const long int SFMT19937_MSK4_ParamSet[NBSET] = {
    0xbffffff6U,    0xfdffffc7U,    0xffef77ffU,    0xf7fdffbeU,
    0xfffffdffU,    0xfabfdfffU,    0xdeffffffU,    0xbfdeffebU,
    0xef7ff21eU,    0xfdfdffbfU,    0xfefeff6eU,    0xfdafebffU,
    0xbfbffeffU,    0xbefbfffdU,    0xf7bffbafU,    0xbdff9efbU,
    0xff9fffdfU,    0xb7dbfdfcU,    0xf74f6f3fU,    0x96ffffdfU,
    0xf7fdddffU,    0xedfefffdU,    0x5ffcffebU,    0xffb7ffdfU,
    0xffdfef7fU,    0xfffbdfb7U,    0xfafffddfU,    0xbffffbffU,
0xbf7bbf7eU,    0xfbffdeffU,    0x6ffaffffU,    0xfffffffeU};
    


// PARITY1 param
const long int SFMT607_PARITY1_ParamSet[NBSET] = {
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,    
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U};

const long int SFMT1279_PARITY1_ParamSet[NBSET] = {
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,    
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U};

const long int SFMT2281_PARITY1_ParamSet[NBSET] = {
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,    
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U};

const long int SFMT4253_PARITY1_ParamSet[NBSET] = {
    0xa8000001U,    0xc0000001U,    0x20000001U,    0x20000001U,
    0xa0000001U,    0x40000001U,    0x40000001U,    0x42000001U,
    0x60000001U,    0xc0000001U,    0xa0000001U,    0xa0000001U,
    0x90000001U,    0x20000001U,    0xa0000001U,    0x1U,
    0x1U,    0x28000001U,    0x80000001U,    0x1U,
    0x10000001U,    0x60000001U,    0x70000001U,    0x1U,   
    0x40000001U,    0xb0000001U,    0xe0000001U,    0x80000001U,    
0x20000001U,    0xc0000001U,    0x40000001U,    0x48000001U};

const long int SFMT11213_PARITY1_ParamSet[NBSET] = {
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,    
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U};

const long int SFMT19937_PARITY1_ParamSet[NBSET] = {
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,
    0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U,    
0x00000001U,    0x00000001U,    0x00000001U,    0x00000001U};


// PARITY2 param
const long int SFMT607_PARITY2_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT1279_PARITY2_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT2281_PARITY2_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT4253_PARITY2_ParamSet[NBSET] = {
    0xaf5390a3U,    0x078d2186U,    0x8b4ecd15U,    0x8ed2d92aU,
    0xd3bdda16U,    0x4e4b2758U,    0x7df3317aU,    0xeb34cfceU,
    0x045bc889U,    0xeb6e2a53U,    0xe4616943U,    0xe4d017d4U,
    0x655f358eU,    0xb081cfbeU,    0x9405b85dU,    0x324cb2feU,
    0x92040000U,    0xf2e5468cU,    0x9026447eU,    0x3725b98fU,
    0xbc129652U,    0x0623be05U,    0xb4dc9c74U,    0x179503c2U,
    0xbec29ed7U,    0xc49c2158U,    0x8ecaba1bU,    0xacaa212fU,
0x8f3c8ae4U,    0x93fcf151U,    0x9ed70b6bU,    0x836a277aU};
    
const long int SFMT11213_PARITY2_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT19937_PARITY2_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};


// PARITY3 param
const long int SFMT607_PARITY3_ParamSet[NBSET] = {
    0x0U,    0x80000000U,    0x0U,    0x10000000U,
    0x20000000U,    0x80000000U,    0x0U,    0x0U,
    0x40000000U,    0x2000000U,    0x80000000U,    0x0U,
    0x80000000U,    0x0U,    0x0U,    0x80000000U,
    0x0U,    0x40000000U,    0x0U,    0x80000000U,
    0x80000000U,    0x0U,    0x0U,    0x0U,
    0x4000000U,    0x80000000U,    0x40000000U,    0x0U,
0x80000000U,    0x0U,    0x0U,    0x0U};

const long int SFMT1279_PARITY3_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT2281_PARITY3_ParamSet[NBSET] = {
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,
    0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U,    
0x00000000U,    0x00000000U,    0x00000000U,    0x00000000U};

const long int SFMT4253_PARITY3_ParamSet[NBSET] = {
    0xb740b3f8U,    0xe690a4a9U,    0xb3480fc1U,    0xefc5d057U,
    0x4140113fU,    0xb998e05aU,    0xbbfeb4c7U,    0x3eab62b1U,
    0x7b26b276U,    0x4550962U,    0x0a9b362cU,    0xe32206c7U,
    0x996adc07U,    0xb3d5e047U,    0xc60478f2U,    0x442f545cU,
    0x6265f31aU,    0x7b320d81U,    0x1f279907U,    0xd6e1adebU,
    0xb4c6341bU,    0x4730c7b7U,    0xfa71d735U,    0x84579cd5U,
    0x722046bcU,    0xdd1db4e7U,    0x4a61645bU,    0xaa5340c9U,
    0xa3cf01c6U,    0x739a3cf7U,    0x661e1ca7U,    0xc4833eb6U};

const long int SFMT11213_PARITY3_ParamSet[NBSET] = {
    0xe8148000U,    0xbcaaa000U,    0x03dc0000U,    0xf73ca000U,
    0xf75e8000U,    0x6f4ea000U,    0x830e0000U,    0x5b202000U,
    0x953e8000U,    0xe1d70000U,    0x8a3d0800U,    0x5c142000U,
    0x68cbe000U,    0x73b20000U,    0xce408000U,    0x0eb18000U,
    0x83409000U,    0xa25c0200U,    0xa0b64800U,    0x76adc000U,
    0x637cd000U,    0x43f60800U,    0x2aa69000U,    0xfe0c8800U,
    0x0443a000U,    0x16d92000U,    0xbca5a000U,    0xf8270000U,
0x22d14000U,    0x16f44800U,    0xa7420000U,    0x42c21400U};

const long int SFMT19937_PARITY3_ParamSet[NBSET] = {
    0x0U,    0x0U,    0x0U,    0x0U,
    0x40000000U,    0x2000000U,    0x0U,    0x40000000U,
    0x0U,    0x0U,    0x0U,    0x0U,
    0x80000000U,    0x0U,    0x0U,    0x0U,
    0x0U,    0x0U,    0x0U,    0x0U,
    0x40000000U,    0x80000000U,    0x0U,    0x0U,
    0x80000000U,    0x0U,    0x0U,    0x40000000U,
0x0U,    0x20000000U,    0x80000000U,    0x0U};
    
    
    
// PARITY4 param
const long int SFMT607_PARITY4_ParamSet[NBSET] = {
    0x5986f054U,    0x30b388afU,    0xb5a64116U,    0x6f6fec6aU,
    0xd63bbe55U,    0xa52af51dU,    0x57810851U,    0x170c7993U,
    0xf4112310U,    0x180e3080U,    0xcd8ababaU,    0x694481ddU,
    0x6e1c11beU,    0x047cd258U,    0xde196133U,    0x946dbc1cU,
    0xa7c70cc8U,    0xe7aab0e7U,    0x7aa354e4U,    0xc824d8c1U,
    0x41129a62U,    0x4925021aU,    0x737556b0U,    0xb4be184dU,
    0xf361b11fU,    0xd28269a7U,    0x39660c35U,    0x18ceeb85U,
    0x41d828efU,    0x2a5c3390U,    0xd99b3c70U,    0x85b22d17U};

const long int SFMT1279_PARITY4_ParamSet[NBSET] = {
    0x40000000U,    0x20000000U,    0x0U,    0x0U,
    0x0U,    0x20000000U,    0x40000000U,    0x80000000U,
    0x40000000U,    0x0U,    0x0U,    0x20000000U,
    0x80000000U,    0x80000000U,    0x40000000U,    0x80000000U,
    0x80000000U,    0x0U,    0x0U,    0x80000000U,
    0x10000000U,    0x80000000U,    0x0U,    0x0U,
    0x80000000U,    0x0U,    0x0U,    0x80000000U,
    0x0U,    0x40000000U,    0x0U,    0x0U};

const long int SFMT2281_PARITY4_ParamSet[NBSET] = {
    0x41dfa600U,    0x92730900U,    0x907c9c00U,    0x0a826800U,
    0x92e7de00U,   0xc66e2000U,    0x1b40e800U,    0x3530d200U,
    0x48acb400U,    0x32455000U,   0xb05f9800U,    0x27b07600U,
    0xc22bf800U,    0x3d350a80U,    0x147a0200U,    0xdb3cce00U,
    0x011ecc00U,    0x07aee200U,    0x197e2080U,    0x2bbd0a00U,
    0x8a50e100U,    0x502bdc20U,    0xdedc0400U,    0x3923e800U,      
    0x8a9b2080U,    0x859df000U,    0xcc1df840U,    0xf6dae320U,
    0xd324a000U,    0xcb8f6a00U,    0x16537800U,    0xc4cbc800U};

const long int SFMT4253_PARITY4_ParamSet[NBSET] = {
    0x6c11486dU,    0x7a5efbe5U,    0x35ab1967U,    0xa6e061d3U,
    0xd458c634U,    0xc4f1b268U,    0x13a23b1dU,    0x95b6f6ebU,
    0xf4e713f6U,    0xe0c0375fU,    0xe8730649U,    0xe65fe0eeU,
    0x9ffd9669U,    0x100e4768U,    0xac65a61dU,    0x69f16b2bU,
    0x68abb978U,    0xd79d5d0dU,    0x070b4438U,    0xdfbc6de4U,
    0x13cb9597U,    0xe4dcb68eU,    0x2b9421bdU,    0x8bcf2f27U,
    0x7d6ac005U,    0x0da08972U,    0x6aac5023U,    0x5ca836d2U,
    0xa91d27ddU,    0x72b647f3U,    0xfd968985U,    0x22b0dc59U};

const long int SFMT11213_PARITY4_ParamSet[NBSET] = { 
    0xd0c7afa3U,    0x278c70beU,    0x1f9394f8U,    0x2ad648b9U,
    0xb53672e0U,    0x1b8db4baU,    0xcd4127afU,    0x95b190fdU,
    0xf7bd34c4U,    0x02717d9dU,    0x026e932dU,    0x0aaefe70U,
    0x1c20bdd1U,    0xb134ef9cU,    0x1344dad1U,    0xafe068e2U,
    0xc4651bf2U,    0xa9cb6a61U,    0xc3969fa3U,    0xdd5c6c66U,
    0xedd65806U,    0x4f337b1dU,    0xa1bd24ddU,    0xceed51f4U,
    0x35321b2aU,    0xacd93bf9U,    0x45e06438U,    0xf6fdf776U,
0xf393e8a4U,    0x24ce4227U,    0xea7b6ca0U,    0xfde64440U};

const long int SFMT19937_PARITY4_ParamSet[NBSET] = { 
    0x13c9e684U,    0xfbdf63e3U,    0xdc0d7984U,    0x0116914cU,
    0x978e01e2U,    0xc9bbd290U,    0x8f2cf308U,    0x2bbdd900U,
    0xdb186980U,    0x21a2acc8U,    0x42530085U,    0x47a2caf4U,
    0x0b6f3e5cU,    0x8eddf8b2U,    0x539ef018U,    0xa631116aU,
    0xfb62abccU,    0x2889855U,    0x921dce22U,    0xda18b200U,
    0x63cd4800U,    0xd71cbdcaU,    0x09d68a18U,    0xc491f160U,
    0xad42c1d1U,    0x7f72515eU,    0x7094862aU,    0x5ee9dc90U,
0xb6a0c2e9U,    0x4bf4dbf0U,    0x43471de2U,    0xc4cda29cU};
    

/* global variables
 * based on line 71-85 of Matsumoto and Saito original code */

/* Mersenne exponent parameters */
int POS1;
int SL1;
int SL2;
int SR1;
int SR2;
long int MSK1;
long int MSK2;
long int MSK3;
long int MSK4; 
uint32_t parity[4];
char IDSTR[70];

/* index of SFMT set of parameters */
int idxSFMT607 = 0;
int idxSFMT1279 = 0;
int idxSFMT2281 = 0;
int idxSFMT4253 = 0;
int idxSFMT11213 = 0;
int idxSFMT19937 = 0;


/* Mersenne Exponent. The period of the sequence 
 *  is a multiple of 2^MEXP-1. */
int mexp = 607;
/* SFMT generator has an internal state array of 128-bit integers,
 * and N is its size. */
int N;
/* N32 is the size of internal state array when regarded as an array
 * of 32-bit integers.*/
int N32;
/* N64 is the size of internal state array when regarded as an array
 * of 64-bit integers.*/
int N64;

//NMAX  defined as  (216091 / 128 + 1) for max MEXPs
#define NMAX 1869

/* based on line 71-85 of Matsumoto and Saito original code */
/* the 128-bit internal state array */
static w128_t sfmt[NMAX];
/* the 32bit integer pointer to the 128-bit internal state array */ 
static uint32_t *psfmt32 = &sfmt[0].u[0];
#if !defined(BIG_ENDIAN64) || defined(ONLY64)
/* the 64bit integer pointer to the 128-bit internal state array */
static uint64_t *psfmt64 = (uint64_t *)&sfmt[0].u[0];
#endif
/** index counter to the 32-bit internal state array */
static int idx;
/** a flag: it is 0 if and only if the internal state is not yet
 * initialized. */
static int initialized = 0;
 

/* init SFMT constant, such as POS1, SL1, etc...*/
void init_SFMT(int mersennexponent, int useparamset)
{
#if defined(HAVE_SSE2)
    {
        //Rprintf("SSE2 supported\n");
    }
#endif
    
    
    switch (mersennexponent) 
    {
        case 607:
            mexp = 607;
            
            if(!useparamset)
                idxSFMT607 = 0;
            
            //Rprintf("1_ %d idx\n", idxSFMT607);
            
            if(idxSFMT607 >= NBSET)
                error(_("internal in initSFMT"));
            
            POS1 = SFMT607_POS1_ParamSet[ idxSFMT607 ];
            SL1 = SFMT607_SL1_ParamSet[ idxSFMT607 ];
            SL2 = SFMT607_SL2_ParamSet[ idxSFMT607 ];
            SR1 = SFMT607_SR1_ParamSet[ idxSFMT607 ];
            SR2 = SFMT607_SR2_ParamSet[ idxSFMT607 ];
            MSK1 = SFMT607_MSK1_ParamSet[ idxSFMT607 ];
            MSK2 = SFMT607_MSK2_ParamSet[ idxSFMT607 ];
            MSK3 = SFMT607_MSK3_ParamSet[ idxSFMT607 ];
            MSK4 = SFMT607_MSK4_ParamSet[ idxSFMT607 ];
            parity[0] = SFMT607_PARITY1_ParamSet[ idxSFMT607 ];
            parity[1] = SFMT607_PARITY2_ParamSet[ idxSFMT607 ];
            parity[2] = SFMT607_PARITY3_ParamSet[ idxSFMT607 ];
            parity[3] = SFMT607_PARITY4_ParamSet[ idxSFMT607 ];
            char temp607[] = "SFMT-607:2-15-3-13-3:fdff37ff-ef7f3f7d-ff777b7d-7ff7fb2f";
            strcpy(IDSTR, temp607);
            
            idxSFMT607 = ( idxSFMT607 + 1 ) % NBSET;
            
            break;
        
        case 1279:
            mexp = 1279;
            
            if(!useparamset)
                idxSFMT1279 = 0;
            
            //Rprintf("%d idx\n", idxSFMT1279);
            
            if(idxSFMT1279 >= NBSET)
                error(_("internal in initSFMT"));
            
            POS1 = SFMT1279_POS1_ParamSet[ idxSFMT1279 ];
            SL1 = SFMT1279_SL1_ParamSet[ idxSFMT1279 ];
            SL2 = SFMT1279_SL2_ParamSet[ idxSFMT1279 ];
            SR1 = SFMT1279_SR1_ParamSet[ idxSFMT1279 ];
            SR2 = SFMT1279_SR2_ParamSet[ idxSFMT1279 ];
            MSK1 = SFMT1279_MSK1_ParamSet[ idxSFMT1279 ];
            MSK2 = SFMT1279_MSK2_ParamSet[ idxSFMT1279 ];
            MSK3 = SFMT1279_MSK3_ParamSet[ idxSFMT1279 ];
            MSK4 = SFMT1279_MSK4_ParamSet[ idxSFMT1279 ];
            parity[0] = SFMT1279_PARITY1_ParamSet[ idxSFMT1279 ];
            parity[1] = SFMT1279_PARITY2_ParamSet[ idxSFMT1279 ];
            parity[2] = SFMT1279_PARITY3_ParamSet[ idxSFMT1279 ];
            parity[3] = SFMT1279_PARITY4_ParamSet[ idxSFMT1279 ];
            char temp1279[] = "SFMT-1279:7-14-3-5-1:f7fefffd-7fefcfff-aff3ef3f-b5ffff7f";
            strcpy(IDSTR, temp1279);
            
            idxSFMT1279 = ( idxSFMT1279 + 1 ) % NBSET;
            
            
            break;
            
        case 2281:
            mexp = 2281;
            
            if(!useparamset)
                idxSFMT2281 = 0;
            
            if(idxSFMT2281 >= NBSET)
                error(_("internal in initSFMT"));
            
            //Rprintf("%d idx\n", idxSFMT2281);
            
            POS1 = SFMT2281_POS1_ParamSet[ idxSFMT2281 ];
            SL1 = SFMT2281_SL1_ParamSet[ idxSFMT2281 ];
            SL2 = SFMT2281_SL2_ParamSet[ idxSFMT2281 ];
            SR1 = SFMT2281_SR1_ParamSet[ idxSFMT2281 ];
            SR2 = SFMT2281_SR2_ParamSet[ idxSFMT2281 ];
            MSK1 = SFMT2281_MSK1_ParamSet[ idxSFMT2281 ];
            MSK2 = SFMT2281_MSK2_ParamSet[ idxSFMT2281 ];
            MSK3 = SFMT2281_MSK3_ParamSet[ idxSFMT2281 ];
            MSK4 = SFMT2281_MSK4_ParamSet[ idxSFMT2281 ];
            parity[0] = SFMT2281_PARITY1_ParamSet[ idxSFMT2281 ];
            parity[1] = SFMT2281_PARITY2_ParamSet[ idxSFMT2281 ];
            parity[2] = SFMT2281_PARITY3_ParamSet[ idxSFMT2281 ];
            parity[3] = SFMT2281_PARITY4_ParamSet[ idxSFMT2281 ];
            char temp2281[] = "SFMT-2281:12-19-1-5-1:bff7ffbf-fdfffffe-f7ffef7f-f2f7cbbf";
            strcpy(IDSTR, temp2281);
            
            idxSFMT2281 = ( idxSFMT2281 + 1 ) % NBSET;
            
            break;
            
        case 4253:
            mexp = 4253;
            
            if(!useparamset)
                idxSFMT4253 = 0;

            if(idxSFMT4253 >= NBSET)
                error(_("internal in initSFMT"));

            //Rprintf("%d idx\n", idxSFMT4253);
            
            POS1 = SFMT4253_POS1_ParamSet[ idxSFMT4253 ];
            SL1 = SFMT4253_SL1_ParamSet[ idxSFMT4253 ];
            SL2 = SFMT4253_SL2_ParamSet[ idxSFMT4253 ];
            SR1 = SFMT4253_SR1_ParamSet[ idxSFMT4253 ];
            SR2 = SFMT4253_SR2_ParamSet[ idxSFMT4253 ];
            MSK1 = SFMT4253_MSK1_ParamSet[ idxSFMT4253 ];
            MSK2 = SFMT4253_MSK2_ParamSet[ idxSFMT4253 ];
            MSK3 = SFMT4253_MSK3_ParamSet[ idxSFMT4253 ];
            MSK4 = SFMT4253_MSK4_ParamSet[ idxSFMT4253 ];
            parity[0] = SFMT4253_PARITY1_ParamSet[ idxSFMT4253 ];
            parity[1] = SFMT4253_PARITY2_ParamSet[ idxSFMT4253 ];
            parity[2] = SFMT4253_PARITY3_ParamSet[ idxSFMT4253 ];
            parity[3] = SFMT4253_PARITY4_ParamSet[ idxSFMT4253 ];
            char temp4253[] = "SFMT-4253:17-20-1-7-1:9f7bffff-9fffff5f-3efffffb-fffff7bb";
            strcpy(IDSTR, temp4253);
            
            idxSFMT4253 = ( idxSFMT4253 + 1 ) % NBSET;
            
            break;
            
        case 11213:
            mexp = 11213;
            
            if(!useparamset)
                idxSFMT11213 = 0;

            if(idxSFMT11213 >= NBSET)
                error(_("internal in initSFMT"));
            
            //Rprintf("%d idx\n", idxSFMT11213);
            
            POS1 = SFMT11213_POS1_ParamSet[ idxSFMT11213 ];
            SL1 = SFMT11213_SL1_ParamSet[ idxSFMT11213 ];
            SL2 = SFMT11213_SL2_ParamSet[ idxSFMT11213 ];
            SR1 = SFMT11213_SR1_ParamSet[ idxSFMT11213 ];
            SR2 = SFMT11213_SR2_ParamSet[ idxSFMT11213 ];
            MSK1 = SFMT11213_MSK1_ParamSet[ idxSFMT11213 ];
            MSK2 = SFMT11213_MSK2_ParamSet[ idxSFMT11213 ];
            MSK3 = SFMT11213_MSK3_ParamSet[ idxSFMT11213 ];
            MSK4 = SFMT11213_MSK4_ParamSet[ idxSFMT11213 ];
            parity[0] = SFMT11213_PARITY1_ParamSet[ idxSFMT11213 ];
            parity[1] = SFMT11213_PARITY2_ParamSet[ idxSFMT11213 ];
            parity[2] = SFMT11213_PARITY3_ParamSet[ idxSFMT11213 ];
            parity[3] = SFMT11213_PARITY4_ParamSet[ idxSFMT11213 ];
            char temp11213[] = "SFMT-11213:68-14-3-7-3:effff7fb-ffffffef-dfdfbfff-7fffdbfd";
            strcpy(IDSTR, temp11213);
            
            idxSFMT11213 = ( idxSFMT11213 + 1 ) % NBSET;
            break;
            
        case 19937:
            mexp = 19937;
            
            if(!useparamset)
                idxSFMT19937 = 0;
            
            if(idxSFMT19937 >= NBSET)
                error(_("internal in initSFMT"));
                
            //Rprintf("%d idx\n", idxSFMT19937);
            
            POS1 = SFMT19937_POS1_ParamSet[ idxSFMT19937 ];
            SL1 = SFMT19937_SL1_ParamSet[ idxSFMT19937 ];
            SL2 = SFMT19937_SL2_ParamSet[ idxSFMT19937 ];
            SR1 = SFMT19937_SR1_ParamSet[ idxSFMT19937 ];
            SR2 = SFMT19937_SR2_ParamSet[ idxSFMT19937 ];
            MSK1 = SFMT19937_MSK1_ParamSet[ idxSFMT19937 ];
            MSK2 = SFMT19937_MSK2_ParamSet[ idxSFMT19937 ];
            MSK3 = SFMT19937_MSK3_ParamSet[ idxSFMT19937 ];
            MSK4 = SFMT19937_MSK4_ParamSet[ idxSFMT19937 ];
            parity[0] = SFMT19937_PARITY1_ParamSet[ idxSFMT19937 ];
            parity[1] = SFMT19937_PARITY2_ParamSet[ idxSFMT19937 ];
            parity[2] = SFMT19937_PARITY3_ParamSet[ idxSFMT19937 ];
            parity[3] = SFMT19937_PARITY4_ParamSet[ idxSFMT19937 ];
            char temp19937[] = "SFMT-19937:122-18-1-11-1:dfffffef-ddfecb7f-bffaffff-bffffff6";
            strcpy(IDSTR, temp19937);
            
            idxSFMT19937 = ( idxSFMT19937 + 1 ) % NBSET;
            break;
            
        case 44497:
            mexp = 44497;
            
            POS1 = 330;
            SL1 = 5;
            SL2 = 3;
            SR1 = 9;
            SR2 = 3;
            MSK1 = 0xeffffffbU;
            MSK2 = 0xdfbebfffU;
            MSK3 = 0xbfbf7befU;
            MSK4 = 0x9ffd7bffU;
            parity[0] = 0x00000001U;
            parity[1] = 0x00000000U; 
            parity[2] = 0xa3ac4000U;
            parity[3] = 0xecc1327aU;
            char temp44497[] = "SFMT-44497:330-5-3-9-3:effffffb-dfbebfff-bfbf7bef-9ffd7bff";
            strcpy(IDSTR, temp44497);
            break;
            
        case 86243:
            mexp = 86243;
            
            POS1 = 366;
            SL1 = 6;
            SL2 = 7;
            SR1 = 19;
            SR2 = 1;
            MSK1 = 0xfdbffbffU;
            MSK2 = 0xbff7ff3fU;
            MSK3 = 0xfd77efffU;
            MSK4 = 0xbf9ff3ffU;
            parity[0] = 0x00000001U;
            parity[1] = 0x00000000U; 
            parity[2] = 0x00000000U;
            parity[3] = 0xe9528d85U;
            char temp86243[] = "SFMT-86243:366-6-7-19-1:fdbffbff-bff7ff3f-fd77efff-bf9ff3ff";
            strcpy(IDSTR, temp86243);
            break;
            
        case 132049:
            mexp = 132049;
            
            POS1 = 110;
            SL1 = 19;
            SL2 = 1;
            SR1 = 21;
            SR2 = 1;
            MSK1 = 0xffffbb5fU;
            MSK2 = 0xfb6ebf95U;
            MSK3 = 0xfffefffaU;
            MSK4 = 0xcff77fffU;
            parity[0] = 0x00000001U;
            parity[1] = 0x00000000U; 
            parity[2] = 0xcb520000U;
            parity[3] = 0xc7e91c7dU;
            char temp132049[] = "SFMT-132049:110-19-1-21-1:ffffbb5f-fb6ebf95-fffefffa-cff77fff";
            strcpy(IDSTR, temp132049);
            break;
        
        case 216091:
            mexp = 216091;
            
            POS1 = 627;
            SL1 = 11;
            SL2 = 3;
            SR1 = 10;
            SR2 = 1;
            MSK1 = 0xbff7bff7U;
            MSK2 = 0xbfffffffU;
            MSK3 = 0xbffffa7fU;
            MSK4 = 0xffddfbfbU;
            parity[0] = 0xf8000001U;
            parity[1] = 0x89e80709U; 
            parity[2] = 0x3bd2b64bU;
            parity[3] = 0x0c64b1e4U;
            char temp216091[] = "SFMT-216091:627-11-3-10-1:bff7bff7-bfffffff-bffffa7f-ffddfbfb";
            strcpy(IDSTR, temp216091);
            break;
            
        default:
            error(_("SF Mersenne Twister wrongly initialized"));
    }
    
    N = (mersennexponent / 128 + 1);
    N32  = (N * 4);
    N64 = (N * 2);    
    
    
}
/* =================== end of my code =============== */


/*----------------
  STATIC FUNCTIONS
  ----------------*/
inline static int idxof(int i);
inline static void rshift128(w128_t *out,  w128_t const *in, int shift);
inline static void lshift128(w128_t *out,  w128_t const *in, int shift);
inline static void gen_rand_all(void);
inline static void gen_rand_array(w128_t *array, int size);
inline static uint32_t func1(uint32_t x);
inline static uint32_t func2(uint32_t x);
static void period_certification(void);
#if defined(BIG_ENDIAN64) && !defined(ONLY64)
inline static void swap(w128_t *array, int size);
#endif

#if defined(HAVE_ALTIVEC)
  #include "SFMT-alti.h"
#elif defined(HAVE_SSE2)
  #include "SFMT-sse2.h"
#endif

/**
 * This function simulate a 64-bit index of LITTLE ENDIAN 
 * in BIG ENDIAN machine.
 */
#ifdef ONLY64
inline static int idxof(int i) {
    return i ^ 1;
}
#else
inline static int idxof(int i) {
    return i;
}
#endif
/**
 * This function simulates SIMD 128-bit right shift by the standard C.
 * The 128-bit integer given in in is shifted by (shift * 8) bits.
 * This function simulates the LITTLE ENDIAN SIMD.
 * @param out the output of this function
 * @param in the 128-bit data to be shifted
 * @param shift the shift value
 */
#ifdef ONLY64
inline static void rshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[2] << 32) | ((uint64_t)in->u[3]);
    tl = ((uint64_t)in->u[0] << 32) | ((uint64_t)in->u[1]);

    oh = th >> (shift * 8);
    ol = tl >> (shift * 8);
    ol |= th << (64 - shift * 8);
    out->u[0] = (uint32_t)(ol >> 32);
    out->u[1] = (uint32_t)ol;
    out->u[2] = (uint32_t)(oh >> 32);
    out->u[3] = (uint32_t)oh;
}
#else
inline static void rshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[3] << 32) | ((uint64_t)in->u[2]);
    tl = ((uint64_t)in->u[1] << 32) | ((uint64_t)in->u[0]);

    oh = th >> (shift * 8);
    ol = tl >> (shift * 8);
    ol |= th << (64 - shift * 8);
    out->u[1] = (uint32_t)(ol >> 32);
    out->u[0] = (uint32_t)ol;
    out->u[3] = (uint32_t)(oh >> 32);
    out->u[2] = (uint32_t)oh;
}
#endif
/**
 * This function simulates SIMD 128-bit left shift by the standard C.
 * The 128-bit integer given in in is shifted by (shift * 8) bits.
 * This function simulates the LITTLE ENDIAN SIMD.
 * @param out the output of this function
 * @param in the 128-bit data to be shifted
 * @param shift the shift value
 */
#ifdef ONLY64
inline static void lshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[2] << 32) | ((uint64_t)in->u[3]);
    tl = ((uint64_t)in->u[0] << 32) | ((uint64_t)in->u[1]);

    oh = th << (shift * 8);
    ol = tl << (shift * 8);
    oh |= tl >> (64 - shift * 8);
    out->u[0] = (uint32_t)(ol >> 32);
    out->u[1] = (uint32_t)ol;
    out->u[2] = (uint32_t)(oh >> 32);
    out->u[3] = (uint32_t)oh;
}
#else
inline static void lshift128(w128_t *out, w128_t const *in, int shift) {
    uint64_t th, tl, oh, ol;

    th = ((uint64_t)in->u[3] << 32) | ((uint64_t)in->u[2]);
    tl = ((uint64_t)in->u[1] << 32) | ((uint64_t)in->u[0]);

    oh = th << (shift * 8);
    ol = tl << (shift * 8);
    oh |= tl >> (64 - shift * 8);
    out->u[1] = (uint32_t)(ol >> 32);
    out->u[0] = (uint32_t)ol;
    out->u[3] = (uint32_t)(oh >> 32);
    out->u[2] = (uint32_t)oh;
}
#endif

/**
 * This function represents the recursion formula.
 * @param r output
 * @param a a 128-bit part of the internal state array
 * @param b a 128-bit part of the internal state array
 * @param c a 128-bit part of the internal state array
 * @param d a 128-bit part of the internal state array
 */
#if (!defined(HAVE_ALTIVEC)) && (!defined(HAVE_SSE2))
#ifdef ONLY64
inline static void do_recursion(w128_t *r, w128_t *a, w128_t *b, w128_t *c,
				w128_t *d) {
    w128_t x;
    w128_t y;

    lshift128(&x, a, SL2);
    rshift128(&y, c, SR2);
    r->u[0] = a->u[0] ^ x.u[0] ^ ((b->u[0] >> SR1) & MSK2) ^ y.u[0] 
	^ (d->u[0] << SL1);
    r->u[1] = a->u[1] ^ x.u[1] ^ ((b->u[1] >> SR1) & MSK1) ^ y.u[1] 
	^ (d->u[1] << SL1);
    r->u[2] = a->u[2] ^ x.u[2] ^ ((b->u[2] >> SR1) & MSK4) ^ y.u[2] 
	^ (d->u[2] << SL1);
    r->u[3] = a->u[3] ^ x.u[3] ^ ((b->u[3] >> SR1) & MSK3) ^ y.u[3] 
	^ (d->u[3] << SL1);
}
#else
inline static void do_recursion(w128_t *r, w128_t *a, w128_t *b, w128_t *c,
				w128_t *d) {
    w128_t x;
    w128_t y;

    lshift128(&x, a, SL2);
    rshift128(&y, c, SR2);
    r->u[0] = a->u[0] ^ x.u[0] ^ ((b->u[0] >> SR1) & MSK1) ^ y.u[0] 
	^ (d->u[0] << SL1);
    r->u[1] = a->u[1] ^ x.u[1] ^ ((b->u[1] >> SR1) & MSK2) ^ y.u[1] 
	^ (d->u[1] << SL1);
    r->u[2] = a->u[2] ^ x.u[2] ^ ((b->u[2] >> SR1) & MSK3) ^ y.u[2] 
	^ (d->u[2] << SL1);
    r->u[3] = a->u[3] ^ x.u[3] ^ ((b->u[3] >> SR1) & MSK4) ^ y.u[3] 
	^ (d->u[3] << SL1);
}
#endif
#endif

#if (!defined(HAVE_ALTIVEC)) && (!defined(HAVE_SSE2))
/**
 * This function fills the internal state array with pseudorandom
 * integers.
 */
inline static void gen_rand_all(void) {
    int i;
    w128_t *r1, *r2;

    r1 = &sfmt[N - 2];
    r2 = &sfmt[N - 1];
    for (i = 0; i < N - POS1; i++) {
	do_recursion(&sfmt[i], &sfmt[i], &sfmt[i + POS1], r1, r2);
	r1 = r2;
	r2 = &sfmt[i];
    }
    for (; i < N; i++) {
	do_recursion(&sfmt[i], &sfmt[i], &sfmt[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &sfmt[i];
    }
}

/**
 * This function fills the user-specified array with pseudorandom
 * integers.
 *
 * @param array an 128-bit array to be filled by pseudorandom numbers.  
 * @param size number of 128-bit pseudorandom numbers to be generated.
 */
inline static void gen_rand_array(w128_t *array, int size) {
    int i, j;
    w128_t *r1, *r2;

    r1 = &sfmt[N - 2];
    r2 = &sfmt[N - 1];
    for (i = 0; i < N - POS1; i++) {
	do_recursion(&array[i], &sfmt[i], &sfmt[i + POS1], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (; i < N; i++) {
	do_recursion(&array[i], &sfmt[i], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (; i < size - N; i++) {
	do_recursion(&array[i], &array[i - N], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
    }
    for (j = 0; j < 2 * N - size; j++) {
	sfmt[j] = array[j + size - N];
    }
    for (; i < size; i++, j++) {
	do_recursion(&array[i], &array[i - N], &array[i + POS1 - N], r1, r2);
	r1 = r2;
	r2 = &array[i];
	sfmt[j] = array[i];
    }
}
#endif

#if defined(BIG_ENDIAN64) && !defined(ONLY64) && !defined(HAVE_ALTIVEC)
inline static void swap(w128_t *array, int size) {
    int i;
    uint32_t x, y;

    for (i = 0; i < size; i++) {
	x = array[i].u[0];
	y = array[i].u[2];
	array[i].u[0] = array[i].u[1];
	array[i].u[2] = array[i].u[3];
	array[i].u[1] = x;
	array[i].u[3] = y;
    }
}
#endif
/**
 * This function represents a function used in the initialization
 * by init_by_array
 * @param x 32-bit integer
 * @return 32-bit integer
 */
static uint32_t func1(uint32_t x) {
    return (x ^ (x >> 27)) * (uint32_t)1664525UL;
}

/**
 * This function represents a function used in the initialization
 * by init_by_array
 * @param x 32-bit integer
 * @return 32-bit integer
 */
static uint32_t func2(uint32_t x) {
    return (x ^ (x >> 27)) * (uint32_t)1566083941UL;
}

/**
 * This function certificate the period of 2^{MEXP}
 */
static void period_certification(void) {
    int inner = 0;
    int i, j;
    uint32_t work;

    for (i = 0; i < 4; i++)
	inner ^= psfmt32[idxof(i)] & parity[i];
    for (i = 16; i > 0; i >>= 1)
	inner ^= inner >> i;
    inner &= 1;
    /* check OK */
    if (inner == 1) {
	return;
    }
    /* check NG, and modification */
    for (i = 0; i < 4; i++) {
	work = 1;
	for (j = 0; j < 32; j++) {
	    if ((work & parity[i]) != 0) {
		psfmt32[idxof(i)] ^= work;
		return;
	    }
	    work = work << 1;
	}
    }
}

/*----------------
  PUBLIC FUNCTIONS
  ----------------*/
/**
 * This function returns the identification string.
 * The string shows the word size, the Mersenne exponent,
 * and all parameters of this generator.
 */
const char *get_idstring(void) {
    return IDSTR;
}

/**
 * This function returns the minimum size of array used for \b
 * fill_array32() function.
 * @return minimum size of array used for fill_array32() function.
 */
int get_min_array_size32(void) {
    return N32;
}

/**
 * This function returns the minimum size of array used for \b
 * fill_array64() function.
 * @return minimum size of array used for fill_array64() function.
 */
int get_min_array_size64(void) {
    return N64;
}

#ifndef ONLY64
/**
 * This function generates and returns 32-bit pseudorandom number.
 * init_gen_rand or init_by_array must be called before this function.
 * @return 32-bit pseudorandom number
 */
uint32_t gen_rand32(void) {
    uint32_t r;

    assert(initialized);
    if (idx >= N32) {
        
        /*
         * code of Christophe Dutang 
         * added to interface with R 
         */
        /* ===================  my code  =================== */    
#if defined(HAVE_SSE2)
        
        switch(mexp)
        {
            case 607:
                //Rprintf("2- %d idx\n", idxSFMT607);
                switch(idxSFMT607)
                {
                    case 1: 
                        gen_rand_all_607_1();                       break;    
                    case 2:
                        gen_rand_all_607_2();                        break;
                    case 3:
                        gen_rand_all_607_3();                        break;
                    case 4:
                        gen_rand_all_607_4();                        break;
                    case 5:
                        gen_rand_all_607_5();                        break;
                    case 6: 
                        gen_rand_all_607_6();                        break;    
                    case 7:
                        gen_rand_all_607_7();                        break;
                    case 8:
                        gen_rand_all_607_8();                        break;
                    case 9:
                        gen_rand_all_607_9();                        break;
                    case 10:
                        gen_rand_all_607_10();                        break;
                    case 11: 
                        gen_rand_all_607_11();                        break;    
                    case 12:
                        gen_rand_all_607_12();                        break;
                    case 13:
                        gen_rand_all_607_13();                        break;
                    case 14:
                        gen_rand_all_607_14();                        break;
                    case 15:
                        gen_rand_all_607_15();                        break;                        
                    case 16: 
                        gen_rand_all_607_16();                        break;    
                    case 17:
                        gen_rand_all_607_17();                        break;
                    case 18:
                        gen_rand_all_607_18();                        break;
                    case 19:
                        gen_rand_all_607_19();                        break;
                    case 20:
                        gen_rand_all_607_20();                        break;                        
                    case 21: 
                        gen_rand_all_607_21();                        break;    
                    case 22:
                        gen_rand_all_607_22();                        break;
                    case 23:
                        gen_rand_all_607_23();                        break;
                    case 24:
                        gen_rand_all_607_24();                        break;
                    case 25:
                        gen_rand_all_607_25();                        break;                        
                    case 26: 
                        gen_rand_all_607_26();                        break;    
                    case 27:
                        gen_rand_all_607_27();                        break;
                    case 28:
                        gen_rand_all_607_28();                        break;
                    case 29:
                        gen_rand_all_607_29();                        break;
                    case 30:
                        gen_rand_all_607_30();                        break;                        
                    case 31:
                        gen_rand_all_607_31();                        break;                        
                    case 0:
                        gen_rand_all_607_32();                        break;                                                
                    default:
                        error(_("wrong index in SFMT - sse2 support"));
                }
                break;
            case 1279:
                gen_rand_all_1279_1();
                break;
            case 2281:
                gen_rand_all_2281_1();
                break;
            case 4253:
                gen_rand_all_4253_1();
                break;
            case 11213:
                gen_rand_all_11213_1();
                break;
            case 19937:
                gen_rand_all_19937_1();
                break;
            case 44497:
                gen_rand_all_44497_1();
                break;
            case 86243:
                gen_rand_all_86243_1();
                break;
            case 132049:
                gen_rand_all_132049_1();
                break;
            case 216091:
                gen_rand_all_216091_1();
                break;                
            default:
                error(_("wrong mersenne exponent - sse2 support"));
        }
#else
        gen_rand_all();
#endif
        
        /* =================== end of my code =============== */
        
	idx = 0;
    }
    r = psfmt32[idx++];
    return r;
}
#endif
/**
 * This function generates and returns 64-bit pseudorandom number.
 * init_gen_rand or init_by_array must be called before this function.
 * The function gen_rand64 should not be called after gen_rand32,
 * unless an initialization is again executed. 
 * @return 64-bit pseudorandom number
 */
uint64_t gen_rand64(void) 
{
#if defined(BIG_ENDIAN64) && !defined(ONLY64)
    uint32_t r1, r2;
#else
    uint64_t r;
#endif

    assert(initialized);
    assert(idx % 2 == 0);

    if (idx >= N32) 
    {
        /*
         * code of Christophe Dutang 
         * added to interface with R 
         */
        /* ===================  my code  =================== */    
#if defined(HAVE_SSE2)
        switch(mexp)
        {
            case 607:
                Rprintf("2 64bit- %d idx\n", idxSFMT607);
                switch(idxSFMT607)
                {
                    case 1: 
                        gen_rand_all_607_1();                       break;    
                    case 2:
                        gen_rand_all_607_2();                        break;
                    case 3:
                        gen_rand_all_607_3();                        break;
                    case 4:
                        gen_rand_all_607_4();                        break;
                    case 5:
                        gen_rand_all_607_5();                        break;
                    case 6: 
                        gen_rand_all_607_6();                        break;    
                    case 7:
                        gen_rand_all_607_7();                        break;
                    case 8:
                        gen_rand_all_607_8();                        break;
                    case 9:
                        gen_rand_all_607_9();                        break;
                    case 10:
                        gen_rand_all_607_10();                        break;
                    case 11: 
                        gen_rand_all_607_11();                        break;    
                    case 12:
                        gen_rand_all_607_12();                        break;
                    case 13:
                        gen_rand_all_607_13();                        break;
                    case 14:
                        gen_rand_all_607_14();                        break;
                    case 15:
                        gen_rand_all_607_15();                        break;                        
                    case 16: 
                        gen_rand_all_607_16();                        break;    
                    case 17:
                        gen_rand_all_607_17();                        break;
                    case 18:
                        gen_rand_all_607_18();                        break;
                    case 19:
                        gen_rand_all_607_19();                        break;
                    case 20:
                        gen_rand_all_607_20();                        break;                        
                    case 21: 
                        gen_rand_all_607_21();                        break;    
                    case 22:
                        gen_rand_all_607_22();                        break;
                    case 23:
                        gen_rand_all_607_23();                        break;
                    case 24:
                        gen_rand_all_607_24();                        break;
                    case 25:
                        gen_rand_all_607_25();                        break;                        
                    case 26: 
                        gen_rand_all_607_26();                        break;    
                    case 27:
                        gen_rand_all_607_27();                        break;
                    case 28:
                        gen_rand_all_607_28();                        break;
                    case 29:
                        gen_rand_all_607_29();                        break;
                    case 30:
                        gen_rand_all_607_30();                        break;                        
                    case 31:
                        gen_rand_all_607_31();                        break;                        
                    case 0:
                        gen_rand_all_607_32();                        break;                                                
                    default:
                        error(_("wrong index in SFMT - sse2 support"));
                }
                break;
            case 1279:
                gen_rand_all_1279_1();
                break;
            case 2281:
                gen_rand_all_2281_1();
                break;
            case 4253:
                gen_rand_all_4253_1();
                break;
            case 11213:
                gen_rand_all_11213_1();
                break;
            case 19937:
                gen_rand_all_19937_1();
                break;
            case 44497:
                gen_rand_all_44497_1();
                break;
            case 86243:
                gen_rand_all_86243_1();
                break;
            case 132049:
                gen_rand_all_132049_1();
                break;
            case 216091:
                gen_rand_all_216091_1();
                break;   
            default:
                error(_("wrong mersenne exponent - sse2 support"));
        }
#else
        gen_rand_all();
#endif
        
        /* =================== end of my code =============== */
        
	idx = 0;
    }
#if defined(BIG_ENDIAN64) && !defined(ONLY64)
    r1 = psfmt32[idx];
    r2 = psfmt32[idx + 1];
    idx += 2;
    return ((uint64_t)r2 << 32) | r1;
#else
    r = psfmt64[idx / 2];
    idx += 2;
    return r;
#endif
}

#ifndef ONLY64
/**
 * This function generates pseudorandom 32-bit integers in the
 * specified array[] by one call. The number of pseudorandom integers
 * is specified by the argument size, which must be at least 624 and a
 * multiple of four.  The generation by this function is much faster
 * than the following gen_rand function.
 *
 * For initialization, init_gen_rand or init_by_array must be called
 * before the first call of this function. This function can not be
 * used after calling gen_rand function, without initialization.
 *
 * @param array an array where pseudorandom 32-bit integers are filled
 * by this function.  The pointer to the array must be \b "aligned"
 * (namely, must be a multiple of 16) in the SIMD version, since it
 * refers to the address of a 128-bit integer.  In the standard C
 * version, the pointer is arbitrary.
 *
 * @param size the number of 32-bit pseudorandom integers to be
 * generated.  size must be a multiple of 4, and greater than or equal
 * to (MEXP / 128 + 1) * 4.
 *
 * @note \b memalign or \b posix_memalign is available to get aligned
 * memory. Mac OSX doesn't have these functions, but \b malloc of OSX
 * returns the pointer to the aligned memory block.
 */
void fill_array32(uint32_t *array, int size) {
    assert(initialized);
    assert(idx == N32);
    assert(size % 4 == 0);
    assert(size >= N32);

    /*
     * code of Christophe Dutang 
     * added to interface with R 
     */
    /* ===================  my code  =================== */    
#if defined(HAVE_SSE2)
   switch(mexp)
    {
        case 607:
            //Rprintf("2- %d idx\n", idxSFMT607);
            switch(idxSFMT607)
            {
                case 1: 
                    gen_rand_array_607_1((w128_t *)array, size / 4); break;    
                case 2:
                    gen_rand_array_607_2((w128_t *)array, size / 4); break;    
                case 3:
                    gen_rand_array_607_3((w128_t *)array, size / 4); break;    
                case 4:
                    gen_rand_array_607_4((w128_t *)array, size / 4); break;    
                case 5:
                    gen_rand_array_607_5((w128_t *)array, size / 4); break;    
                case 6: 
                    gen_rand_array_607_6((w128_t *)array, size / 4); break;    
                case 7:
                    gen_rand_array_607_7((w128_t *)array, size / 4); break;    
                case 8:
                    gen_rand_array_607_8((w128_t *)array, size / 4); break;    
                case 9:
                    gen_rand_array_607_9((w128_t *)array, size / 4); break;    
                case 10:
                    gen_rand_array_607_10((w128_t *)array, size / 4); break;    
                case 11: 
                    gen_rand_array_607_11((w128_t *)array, size / 4); break;    
                case 12:
                    gen_rand_array_607_12((w128_t *)array, size / 4); break;    
                case 13:
                    gen_rand_array_607_13((w128_t *)array, size / 4); break;    
                case 14:
                    gen_rand_array_607_14((w128_t *)array, size / 4); break;    
                case 15:
                    gen_rand_array_607_15((w128_t *)array, size / 4); break;    
                case 16: 
                    gen_rand_array_607_16((w128_t *)array, size / 4); break;    
                case 17:
                    gen_rand_array_607_17((w128_t *)array, size / 4); break;    
                case 18:
                    gen_rand_array_607_18((w128_t *)array, size / 4); break;    
                case 19:
                    gen_rand_array_607_19((w128_t *)array, size / 4); break;    
                case 20:
                    gen_rand_array_607_20((w128_t *)array, size / 4); break;    
                case 21: 
                    gen_rand_array_607_21((w128_t *)array, size / 4); break;    
                case 22:
                    gen_rand_array_607_22((w128_t *)array, size / 4); break;    
                case 23:
                    gen_rand_array_607_23((w128_t *)array, size / 4); break;    
                case 24:
                    gen_rand_array_607_24((w128_t *)array, size / 4); break;    
                case 25:
                    gen_rand_array_607_25((w128_t *)array, size / 4); break;    
                case 26: 
                    gen_rand_array_607_26((w128_t *)array, size / 4); break;    
                case 27:
                    gen_rand_array_607_27((w128_t *)array, size / 4); break;    
                case 28:
                    gen_rand_array_607_28((w128_t *)array, size / 4); break;    
                case 29:
                    gen_rand_array_607_29((w128_t *)array, size / 4); break;    
                case 30:
                    gen_rand_array_607_30((w128_t *)array, size / 4); break;    
                case 31:
                    gen_rand_array_607_31((w128_t *)array, size / 4); break;    
                case 0:
                    gen_rand_array_607_32((w128_t *)array, size / 4); break;                           
                default:
                    error(_("wrong index in SFMT - sse2 support"));
            }
            break;
        case 1279:
            gen_rand_array_1279_1((w128_t *)array, size / 4);
            break;
        case 2281:
            gen_rand_array_607_1((w128_t *)array, size / 4);
            break;
        case 4253:
            gen_rand_array_4253_1((w128_t *)array, size / 4);
            break;
        case 11213:
            gen_rand_array_11213_1((w128_t *)array, size / 4);
            break;
        case 19937:
            gen_rand_array_19937_1((w128_t *)array, size / 4);
            break;
        case 44497:
            gen_rand_array_44497_1((w128_t *)array, size / 4);
            break;
        case 86243:
            gen_rand_array_86243_1((w128_t *)array, size / 4);
            break;
        case 132049:
            gen_rand_array_132049_1((w128_t *)array, size / 4);
            break;
        case 216091:
            gen_rand_array_216091_1((w128_t *)array, size / 4);
            break;   
        default:
            error(_("wrong mersenne exponent - sse2 support"));
    }
#else
    gen_rand_array((w128_t *)array, size / 4);
#endif
    
    /* =================== end of my code =============== */
    
    idx = N32;
}
#endif

/**
 * This function generates pseudorandom 64-bit integers in the
 * specified array[] by one call. The number of pseudorandom integers
 * is specified by the argument size, which must be at least 312 and a
 * multiple of two.  The generation by this function is much faster
 * than the following gen_rand function.
 *
 * For initialization, init_gen_rand or init_by_array must be called
 * before the first call of this function. This function can not be
 * used after calling gen_rand function, without initialization.
 *
 * @param array an array where pseudorandom 64-bit integers are filled
 * by this function.  The pointer to the array must be "aligned"
 * (namely, must be a multiple of 16) in the SIMD version, since it
 * refers to the address of a 128-bit integer.  In the standard C
 * version, the pointer is arbitrary.
 *
 * @param size the number of 64-bit pseudorandom integers to be
 * generated.  size must be a multiple of 2, and greater than or equal
 * to (MEXP / 128 + 1) * 2
 *
 * @note \b memalign or \b posix_memalign is available to get aligned
 * memory. Mac OSX doesn't have these functions, but \b malloc of OSX
 * returns the pointer to the aligned memory block.
 */
void fill_array64(uint64_t *array, int size) {
    assert(initialized);
    assert(idx == N32);
    assert(size % 2 == 0);
    assert(size >= N64);

    /*
     * code of Christophe Dutang 
     * added to interface with R 
     */
    /* ===================  my code  =================== */    
#if defined(HAVE_SSE2)
    switch(mexp)
    {
        case 607:
            Rprintf("2 64bit- %d idx\n", idxSFMT607);
            switch(idxSFMT607)
            {
                case 1: 
                    gen_rand_array_607_1((w128_t *)array, size / 2); break;    
                case 2:
                    gen_rand_array_607_2((w128_t *)array, size / 2); break;    
                case 3:
                    gen_rand_array_607_3((w128_t *)array, size / 2); break;    
                case 4:
                    gen_rand_array_607_4((w128_t *)array, size / 2); break;    
                case 5:
                    gen_rand_array_607_5((w128_t *)array, size / 2); break;    
                case 6: 
                    gen_rand_array_607_6((w128_t *)array, size / 2); break;    
                case 7:
                    gen_rand_array_607_7((w128_t *)array, size / 2); break;    
                case 8:
                    gen_rand_array_607_8((w128_t *)array, size / 2); break;    
                case 9:
                    gen_rand_array_607_9((w128_t *)array, size / 2); break;    
                case 10:
                    gen_rand_array_607_10((w128_t *)array, size / 2); break;    
                case 11: 
                    gen_rand_array_607_11((w128_t *)array, size / 2); break;    
                case 12:
                    gen_rand_array_607_12((w128_t *)array, size / 2); break;    
                case 13:
                    gen_rand_array_607_13((w128_t *)array, size / 2); break;    
                case 14:
                    gen_rand_array_607_14((w128_t *)array, size / 2); break;    
                case 15:
                    gen_rand_array_607_15((w128_t *)array, size / 2); break;    
                case 16: 
                    gen_rand_array_607_16((w128_t *)array, size / 2); break;    
                case 17:
                    gen_rand_array_607_17((w128_t *)array, size / 2); break;    
                case 18:
                    gen_rand_array_607_18((w128_t *)array, size / 2); break;    
                case 19:
                    gen_rand_array_607_19((w128_t *)array, size / 2); break;    
                case 20:
                    gen_rand_array_607_20((w128_t *)array, size / 2); break;    
                case 21: 
                    gen_rand_array_607_21((w128_t *)array, size / 2); break;    
                case 22:
                    gen_rand_array_607_22((w128_t *)array, size / 2); break;    
                case 23:
                    gen_rand_array_607_23((w128_t *)array, size / 2); break;    
                case 24:
                    gen_rand_array_607_24((w128_t *)array, size / 2); break;    
                case 25:
                    gen_rand_array_607_25((w128_t *)array, size / 2); break;    
                case 26: 
                    gen_rand_array_607_26((w128_t *)array, size / 2); break;    
                case 27:
                    gen_rand_array_607_27((w128_t *)array, size / 2); break;    
                case 28:
                    gen_rand_array_607_28((w128_t *)array, size / 2); break;    
                case 29:
                    gen_rand_array_607_29((w128_t *)array, size / 2); break;    
                case 30:
                    gen_rand_array_607_30((w128_t *)array, size / 2); break;    
                case 31:
                    gen_rand_array_607_31((w128_t *)array, size / 2); break;    
                case 0:
                    gen_rand_array_607_32((w128_t *)array, size / 2); break;                           
                default:
                    error(_("wrong index in SFMT - sse2 support"));
            }
            break;
        case 1279:
            gen_rand_array_1279_1((w128_t *)array, size / 2);
            break;
        case 2281:
            gen_rand_array_607_1((w128_t *)array, size / 2);
            break;
        case 4253:
            gen_rand_array_4253_1((w128_t *)array, size / 2);
            break;
        case 11213:
            gen_rand_array_11213_1((w128_t *)array, size / 2);
            break;
        case 19937:
            gen_rand_array_19937_1((w128_t *)array, size / 2);
            break;
        case 44497:
            gen_rand_array_44497_1((w128_t *)array, size / 2);
            break;
        case 86243:
            gen_rand_array_86243_1((w128_t *)array, size / 2);
            break;
        case 132049:
            gen_rand_array_132049_1((w128_t *)array, size / 2);
            break;
        case 216091:
            gen_rand_array_216091_1((w128_t *)array, size / 2);
            break;               
        default:
            error(_("wrong mersenne exponent - sse2 support"));
    }
#else
    gen_rand_array((w128_t *)array, size / 2);
#endif
    
    /* =================== end of my code =============== */
    
    idx = N32;

#if defined(BIG_ENDIAN64) && !defined(ONLY64)
    swap((w128_t *)array, size /2);
#endif
}

/**
 * This function initializes the internal state array with a 32-bit
 * integer seed.
 *
 * @param seed a 32-bit integer used as the seed.
 */
void init_gen_rand(uint32_t seed) {
    int i;

    psfmt32[idxof(0)] = seed;
    for (i = 1; i < N32; i++) {
	psfmt32[idxof(i)] = 1812433253UL * (psfmt32[idxof(i - 1)] 
					    ^ (psfmt32[idxof(i - 1)] >> 30))
	    + i;
    }
    idx = N32;
    period_certification();
    initialized = 1;
}

/**
 * This function initializes the internal state array,
 * with an array of 32-bit integers used as the seeds
 * @param init_key the array of 32-bit integers, used as a seed.
 * @param key_length the length of init_key.
 */
void init_by_array(uint32_t *init_key, int key_length) {
    int i, j, count;
    uint32_t r;
    int lag;
    int mid;
    int size = N * 4;

    if (size >= 623) {
	lag = 11;
    } else if (size >= 68) {
	lag = 7;
    } else if (size >= 39) {
	lag = 5;
    } else {
	lag = 3;
    }
    mid = (size - lag) / 2;

    memset(sfmt, 0x8b, sizeof(sfmt));
    if (key_length + 1 > N32) {
	count = key_length + 1;
    } else {
	count = N32;
    }
    r = func1(psfmt32[idxof(0)] ^ psfmt32[idxof(mid)] 
	      ^ psfmt32[idxof(N32 - 1)]);
    psfmt32[idxof(mid)] += r;
    r += key_length;
    psfmt32[idxof(mid + lag)] += r;
    psfmt32[idxof(0)] = r;

    count--;
    for (i = 1, j = 0; (j < count) && (j < key_length); j++) {
	r = func1(psfmt32[idxof(i)] ^ psfmt32[idxof((i + mid) % N32)] 
		  ^ psfmt32[idxof((i + N32 - 1) % N32)]);
	psfmt32[idxof((i + mid) % N32)] += r;
	r += init_key[j] + i;
	psfmt32[idxof((i + mid + lag) % N32)] += r;
	psfmt32[idxof(i)] = r;
	i = (i + 1) % N32;
    }
    for (; j < count; j++) {
	r = func1(psfmt32[idxof(i)] ^ psfmt32[idxof((i + mid) % N32)] 
		  ^ psfmt32[idxof((i + N32 - 1) % N32)]);
	psfmt32[idxof((i + mid) % N32)] += r;
	r += i;
	psfmt32[idxof((i + mid + lag) % N32)] += r;
	psfmt32[idxof(i)] = r;
	i = (i + 1) % N32;
    }
    for (j = 0; j < N32; j++) {
	r = func2(psfmt32[idxof(i)] + psfmt32[idxof((i + mid) % N32)] 
		  + psfmt32[idxof((i + N32 - 1) % N32)]);
	psfmt32[idxof((i + mid) % N32)] ^= r;
	r -= i;
	psfmt32[idxof((i + mid + lag) % N32)] ^= r;
	psfmt32[idxof(i)] = r;
	i = (i + 1) % N32;
    }

    idx = N32;
    period_certification();
    initialized = 1;
}
