/** 
 * @file  SFMT-sse2.h
 * @brief SIMD oriented Fast Mersenne Twister(SFMT) for Intel SSE2
 *
 * @author Mutsuo Saito (Hiroshima University)
 * @author Makoto Matsumoto (Hiroshima University)
 *
 * @note We assume LITTLE ENDIAN in this file
 *
 * Copyright (C) 2006, 2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
 * University. All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
 *  University. All rights reserved.
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

#ifndef SFMT_SSE2_607_15_H
#define SFMT_SSE2_607_15_H

/*
 * code of Christophe Dutang 
 * added to interface with R 
 */
/* ===================  my code  =================== */
/* these pre-processor define can NOT be const int but they must be 
 defined (as follows) since functions _mm_srli_si128 and 
 _mm_srli_epi32 requires immediate values. */

#define POS1_607_15 1
#define SL1_607_15	23
#define SL2_607_15	1
#define SR1_607_15	20
#define SR2_607_15	1

/* =================== end of my code =============== */

PRE_ALWAYS static __m128i mm_recursion_607_15(__m128i *a, __m128i *b, __m128i c,
				   __m128i d, __m128i mask) ALWAYSINLINE;

/**
 * This function represents the recursion formula.
 * @param a a 128-bit part of the internal state array
 * @param b a 128-bit part of the internal state array
 * @param c a 128-bit part of the internal state array
 * @param d a 128-bit part of the internal state array
 * @param mask 128-bit mask
 * @return output
 */
PRE_ALWAYS static __m128i mm_recursion_607_15(__m128i *a, __m128i *b, 
				   __m128i c, __m128i d, __m128i mask) {
    __m128i v, x, y, z;
    
    x = _mm_load_si128(a);
    y = _mm_srli_epi32(*b, SR1_607_15);
    z = _mm_srli_si128(c, SR2_607_15);
    v = _mm_slli_epi32(d, SL1_607_15);
    z = _mm_xor_si128(z, x);
    z = _mm_xor_si128(z, v);
    x = _mm_slli_si128(x, SL2_607_15);
    y = _mm_and_si128(y, mask);
    z = _mm_xor_si128(z, x);
    z = _mm_xor_si128(z, y);
    return z;
}

/**
 * This function fills the internal state array with pseudorandom
 * integers.
 */
inline static void gen_rand_all_607_15(void) {
    int i;
    __m128i r, r1, r2, mask;
    mask = _mm_set_epi32(MSK4, MSK3, MSK2, MSK1);    
    
    r1 = _mm_load_si128(&sfmt[N - 2].si);
    r2 = _mm_load_si128(&sfmt[N - 1].si);
    for (i = 0; i < N - POS1_607_15; i++) {
	r = mm_recursion_607_15(&sfmt[i].si, &sfmt[i + POS1_607_15].si, r1, r2, mask);
	_mm_store_si128(&sfmt[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (; i < N; i++) {
	r = mm_recursion_607_15(&sfmt[i].si, &sfmt[i + POS1_607_15 - N].si, r1, r2, mask);
	_mm_store_si128(&sfmt[i].si, r);
	r1 = r2;
	r2 = r;
    }
}

/**
 * This function fills the user-specified array with pseudorandom
 * integers.
 *
 * @param array an 128-bit array to be filled by pseudorandom numbers.  
 * @param size number of 128-bit pesudorandom numbers to be generated.
 */
inline static void gen_rand_array_607_15(w128_t *array, int size) {
    int i, j;
    __m128i r, r1, r2, mask;
    mask = _mm_set_epi32(MSK4, MSK3, MSK2, MSK1);

    r1 = _mm_load_si128(&sfmt[N - 2].si);
    r2 = _mm_load_si128(&sfmt[N - 1].si);
    for (i = 0; i < N - POS1_607_15; i++) {
	r = mm_recursion_607_15(&sfmt[i].si, &sfmt[i + POS1_607_15].si, r1, r2, mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (; i < N; i++) {
	r = mm_recursion_607_15(&sfmt[i].si, &array[i + POS1_607_15 - N].si, r1, r2, mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    /* main loop */
    for (; i < size - N; i++) {
	r = mm_recursion_607_15(&array[i - N].si, &array[i + POS1_607_15 - N].si, r1, r2,
			 mask);
	_mm_store_si128(&array[i].si, r);
	r1 = r2;
	r2 = r;
    }
    for (j = 0; j < 2 * N - size; j++) {
	r = _mm_load_si128(&array[j + size - N].si);
	_mm_store_si128(&sfmt[j].si, r);
    }
    for (; i < size; i++) {
	r = mm_recursion_607_15(&array[i - N].si, &array[i + POS1_607_15 - N].si, r1, r2,
			 mask);
	_mm_store_si128(&array[i].si, r);
	_mm_store_si128(&sfmt[j++].si, r);
	r1 = r2;
	r2 = r;
    }
}

#endif
