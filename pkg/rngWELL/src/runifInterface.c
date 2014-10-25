/** 
 * @file  runifInterface.c
 * @brief C file for 'runif' interface
 *
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2009, Petr Savicky, Academy of Sciences of the Czech Republic. 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2009 Petr Savicky, Academy of Sciences of the Czech Republic. 
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
 *  runif interface
 *    
 *      C file
 *
 */

#include "runifInterface.h"


typedef void(*UserUnifSetGeneratorType)(int gener, void (*selected_init)(unsigned int), double (*selected_rand)());

#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
int order, version;

UserUnifSetGeneratorType user_unif_set_generator;

void WELL_get_set_entry_point(UserUnifSetGeneratorType p_user_unif_set_generator)
{
	user_unif_set_generator = p_user_unif_set_generator;
}

void initMT2002(unsigned int *seed, int *n, unsigned int *state)
{
	int i;
	state[0] = *seed;
	for (i = 1; i < *n; i++)
	{
		state[i] = 1812433253UL * ( state[i - 1] ^ ( state[i - 1] >> 30 ) ) + i;
	}
}

void seedWELLRNG512a(unsigned int seed)
{
	int n=16;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG512a( seedArray );
}

void seedWELLRNG521a(unsigned int seed)
{
	int n=17;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG521a( seedArray );
}

void seedWELLRNG521b(unsigned int seed)
{
	int n=17;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG521b( seedArray );
}

void seedWELLRNG607a(unsigned int seed)
{
	int n=19;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG607a( seedArray );
}

void seedWELLRNG607b(unsigned int seed)
{
	int n=19;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG607b( seedArray );
}

void seedWELLRNG800a(unsigned int seed)
{
	int n=25;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG800a( seedArray );
}

void seedWELLRNG800b(unsigned int seed)
{
	int n=25;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG800b( seedArray );
}

void seedWELLRNG1024a(unsigned int seed)
{
	int n=32;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG1024a( seedArray );
}

void seedWELLRNG1024b(unsigned int seed)
{
	int n=32;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG1024b( seedArray );
}

void seedWELLRNG19937a(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937a( seedArray );
}

void seedWELLRNG19937c(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937c( seedArray );
}

void seedWELLRNG19937b(unsigned int seed)
{
	int n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937b( seedArray );
}

void seedWELLRNG21701a(unsigned int seed)
{
	int n=679;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG21701a( seedArray );
}

void seedWELLRNG23209a(unsigned int seed)
{
	int n=726;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG23209a( seedArray );
}

void seedWELLRNG23209b(unsigned int seed)
{
	int n=726;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG23209b( seedArray );
}

void seedWELLRNG44497a(unsigned int seed)
{
	int n=1391;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG44497a( seedArray );
}

void seedWELLRNG44497b(unsigned int seed)
{
	int n=1391;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG44497b( seedArray );
}

double generateWELLRNG512a()
{
	return WELLRNG512a();
}

double generateWELLRNG521a()
{
	return WELLRNG521a();
}

double generateWELLRNG521b()
{
	return WELLRNG521b();
}

double generateWELLRNG607a()
{
	return WELLRNG607a();
}

double generateWELLRNG607b()
{
	return WELLRNG607b();
}

double generateWELLRNG800a()
{
	return WELLRNG800a();
}

double generateWELLRNG800b()
{
	return WELLRNG800b();
}

double generateWELLRNG1024a()
{
	return WELLRNG1024a();
}

double generateWELLRNG1024b()
{
	return WELLRNG1024b();
}

double generateWELLRNG19937a()
{
	return WELLRNG19937a();
}

double generateWELLRNG19937c()
{
	return WELLRNG19937c();
}

double generateWELLRNG19937b()
{
	return WELLRNG19937b();
}

double generateWELLRNG21701a()
{
	return WELLRNG21701a();
}

double generateWELLRNG23209a()
{
	return WELLRNG23209a();
}

double generateWELLRNG23209b()
{
	return WELLRNG23209b();
}

double generateWELLRNG44497a()
{
	return WELLRNG44497a();
}

double generateWELLRNG44497b()
{
	return WELLRNG44497b();
}

// put state functions

void putRngWELL512(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG512a( state );
		user_unif_set_generator(2, seedWELLRNG512a, generateWELLRNG512a);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL521(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG521a( state );
		user_unif_set_generator(2, seedWELLRNG521a, generateWELLRNG521a);
		break;
	case 2:
		InitWELLRNG521b( state );
		user_unif_set_generator(2, seedWELLRNG521b, generateWELLRNG521b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL607(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG607a( state );
		user_unif_set_generator(2, seedWELLRNG607a, generateWELLRNG607a);
		break;
	case 2:
		InitWELLRNG607b( state );
		user_unif_set_generator(2, seedWELLRNG607b, generateWELLRNG607b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL800(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG800a( state );
		user_unif_set_generator(2, seedWELLRNG800a, generateWELLRNG800a);
		break;
	case 2:
		InitWELLRNG800b( state );
		user_unif_set_generator(2, seedWELLRNG800b, generateWELLRNG800b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL1024(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG1024a( state );
		user_unif_set_generator(2, seedWELLRNG1024a, generateWELLRNG1024a);
		break;
	case 2:
		InitWELLRNG1024b( state );
		user_unif_set_generator(2, seedWELLRNG1024b, generateWELLRNG1024b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL19937(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG19937a( state );
		user_unif_set_generator(2, seedWELLRNG19937a, generateWELLRNG19937a);
		break;
	case 2:
		InitWELLRNG19937b( state );
		user_unif_set_generator(2, seedWELLRNG19937b, generateWELLRNG19937b);
		break;
	case 3:
		InitWELLRNG19937c( state );
		user_unif_set_generator(2, seedWELLRNG19937c, generateWELLRNG19937c);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));		
	}
}

void putRngWELL21701(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG21701a( state );
		user_unif_set_generator(2, seedWELLRNG21701a, generateWELLRNG21701a);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));									
	}
}

void putRngWELL23209(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG23209a( state );
		user_unif_set_generator(2, seedWELLRNG23209a, generateWELLRNG23209a);
		break;
	case 2:
		InitWELLRNG23209b( state );
		user_unif_set_generator(2, seedWELLRNG23209b, generateWELLRNG23209b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));									
	}
}

void putRngWELL44497(unsigned int *state)
{
	switch (version)
	{
	case 1:
		InitWELLRNG44497a( state );
		user_unif_set_generator(2, seedWELLRNG44497a, generateWELLRNG44497a);
		break;
	case 2:
		InitWELLRNG44497b( state );
		user_unif_set_generator(2, seedWELLRNG44497b, generateWELLRNG44497b);
		break;
	//default: error(_("wrong version in putRngWELLXXX"));						
	}
}

void putRngWELL(int *porder, int *pversion, unsigned int *state)
{
	order = *porder;
	version = *pversion;
	switch (order)
	{
		case 512: putRngWELL512(state); break;
		case 521: putRngWELL521(state); break;
		case 607: putRngWELL607(state); break;
		case 800: putRngWELL800(state); break;
		case 1024: putRngWELL1024(state); break;
		case 19937: putRngWELL19937(state); break;
		case 21701: putRngWELL21701(state); break;
		case 23209: putRngWELL23209(state); break;
		case 44497: putRngWELL44497(state); break;
        //default: error(_("wrong order in putRngWELL"));			
	}
}

// get state functions

void getRngWELL512(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG512a( state );
	//default: error(_("wrong version in getRngWELLXXX"));		
	}
}

void getRngWELL521(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG521a( state );
		break;
	case 2:
		GetWELLRNG521b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL607(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG607a( state );
		break;
	case 2:
		GetWELLRNG607b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL800(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG800a( state );
		break;
	case 2:
		GetWELLRNG800b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL1024(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG1024a( state );
		break;
	case 2:
		GetWELLRNG1024b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL19937(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG19937a( state );
		break;
	case 2:
		GetWELLRNG19937b( state );
		break;
	case 3:
		GetWELLRNG19937c( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL21701(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG21701a( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL23209(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG23209a( state );
		break;
	case 2:
		GetWELLRNG23209b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL44497(unsigned int *state)
{
	switch (version)
	{
	case 1:
		GetWELLRNG44497a( state );
		break;
	case 2:
		GetWELLRNG44497b( state );
	//default: error(_("wrong version in getRngWELLXXX"));			
	}
}

void getRngWELL(int *porder, int *pversion, unsigned int *state)
{
	*porder = order;
	*pversion = version;
	switch (order)
	{
		case 512: getRngWELL512(state); break;
		case 521: getRngWELL521(state); break;
		case 607: getRngWELL607(state); break;
		case 800: getRngWELL800(state); break;
		case 1024: getRngWELL1024(state); break;
		case 19937: getRngWELL19937(state); break;
		case 21701: getRngWELL21701(state); break;
		case 23209: getRngWELL23209(state); break;
		case 44497: getRngWELL44497(state); break;
		//default: error(_("wrong order in getRngWELL"));		
	}
}

