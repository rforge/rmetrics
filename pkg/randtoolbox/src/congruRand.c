/** 
 * @file  congruRand.c
 * @brief C file for congruential RNG
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2009, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2009 Christophe Dutang, Petr Savicky. 
 * All rights reserved.
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
 *          - Neither the name of the Academy of Sciences of the Czech Republic
 *          nor the names of its contributors may be used to endorse or promote 
 *          products derived from this software without specific prior written
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
/*****************************************************************************
 *  Congruential random number generators
 *    
 *      C file
 *
 */


#include "congruRand.h"
#include "runifInterface.h"

// general linear congruential generator

unsigned long long mod, mult, incr, congru_seed;

// possible value of user_unif_rand_selected in runifInterface.c
double user_unif_rand_congru()
{
	double x;
	congru_seed  = (mult * congru_seed + incr) % mod;
	x = (double) congru_seed / (double) mod;
	if (x == 0.0) {
		x = 0.5 / (double) mod;
	}
	return x;
}

// possible value of user_unif_init_selected in runifInterface.c
void user_unif_init_congru(unsigned int seed)
{
	congru_seed = seed;
}

// called from randtoolbox.c
double get_congruRand()
{
	double x;
	congru_seed  = (mult * congru_seed + incr) % mod;
	x = (double) congru_seed / (double) mod;
	if (x == 0.0) {
		x = 1.0;
	}
	return x;
}

// check several criteria on parameters
int check_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed)
{
	int ok;
	ok = 0 < inp_mult && inp_mult < inp_mod && inp_incr < inp_mod;
	if (!ok) return 1;
	ok = inp_mod - 1 <= (18446744073709551615ULL - inp_incr) / inp_mult;
	if (!ok) return 2;
	ok = inp_seed < inp_mod;
	if (!ok) return 3;
	ok = (inp_mult * inp_seed + inp_incr) % inp_mod != inp_seed;
	if (!ok) return 4;
	return 0;
}

// set parameters
void set_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed)
{
	mod = inp_mod;
	mult = inp_mult;
	incr = inp_incr;
	congru_seed = inp_seed;
}

// get seed
void get_seed_congruRand(unsigned long long *out_seed)
{
	*out_seed = congru_seed;
}

// .C entry point
void get_state_congru(char **params, char **seed)
{
	sprintf(params[0], "%lld", mod);
	sprintf(params[1], "%lld", mult);
	sprintf(params[2], "%lld", incr);
	sprintf(seed[0], "%lld", congru_seed);
}

// .C entry point
void check_state_congru(char **params, char **seed, int *err)
{
	unsigned long long inp_mod, inp_mult, inp_incr, inp_seed;
	sscanf(params[0], "%lld", &inp_mod);
	sscanf(params[1], "%lld", &inp_mult);
	sscanf(params[2], "%lld", &inp_incr);
	sscanf(seed[0], "%lld", &inp_seed);
	*err = check_congruRand(inp_mod, inp_mult, inp_incr, inp_seed);
}

// .C entry point
void put_state_congru(char **params, char **seed)
{
	unsigned long long inp_mod, inp_mult, inp_incr, inp_seed;
	sscanf(params[0], "%lld", &inp_mod);
	sscanf(params[1], "%lld", &inp_mult);
	sscanf(params[2], "%lld", &inp_incr);
	sscanf(seed[0], "%lld", &inp_seed);
	set_congruRand(inp_mod, inp_mult, inp_incr, inp_seed);
	user_unif_rand_selected = user_unif_rand_congru;
}

