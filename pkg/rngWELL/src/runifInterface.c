#include <R.h>
#include "WELL512a.h"
#include "WELL521a.h"
#include "WELL521b.h"
#include "WELL607a.h"
#include "WELL607b.h"
#include "WELL800a.h"
#include "WELL800b.h"
#include "WELL1024a.h"
#include "WELL1024b.h"
#include "WELL19937a.h"
#include "WELL19937b.h"
#include "WELL21701a.h"
#include "WELL23209a.h"
#include "WELL23209b.h"
#include "WELL44497a.h"

#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
int order, version, temp;

void (*user_unif_set_generator)(int gener, void * selected_init, void * selected_rand);

void WELL_get_set_entry_point(void * p_user_unif_set_generator)
{
	user_unif_set_generator = (void (*)(int, void *, void *)) p_user_unif_set_generator;
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
	int i, n=16;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG512a( seedArray );
}

void seedWELLRNG521a(unsigned int seed)
{
	int i, n=17;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG521a( seedArray );
}

void seedWELLRNG607a(unsigned int seed)
{
	int i, n=19;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG607a( seedArray );
}

void seedWELLRNG800a(unsigned int seed)
{
	int i, n=25;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG800a( seedArray );
}

void seedWELLRNG1024a(unsigned int seed)
{
	int i, n=32;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG1024a( seedArray );
}

void seedWELLRNG19937a(unsigned int seed)
{
	int i, n=624;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG19937a( seedArray );
}

void seedWELLRNG21701a(unsigned int seed)
{
	int i, n=679;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG21701a( seedArray );
}

void seedWELLRNG23209a(unsigned int seed)
{
	int i, n=726;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG23209a( seedArray );
}

void seedWELLRNG44497a(unsigned int seed)
{
	int i, n=1391;
	initMT2002(&seed, &n, seedArray);
	InitWELLRNG44497a( seedArray );
}

double generateWELLRNG512a()
{
	return WELLRNG512a();
}

double generateWELLRNG521a()
{
	return WELLRNG521a();
}

double generateWELLRNG607a()
{
	return WELLRNG607a();
}

double generateWELLRNG800a()
{
	return WELLRNG800a();
}

double generateWELLRNG1024a()
{
	return WELLRNG1024a();
}

double generateWELLRNG19937a()
{
	return WELLRNG19937a();
}

double generateWELLRNG21701a()
{
	return WELLRNG21701a();
}

double generateWELLRNG23209a()
{
	return WELLRNG23209a();
}

double generateWELLRNG44497a()
{
	return WELLRNG44497a();
}

// put state functions

void putRngWELL512(unsigned int *state)
{
	InitWELLRNG512a( state );
	user_unif_set_generator(2, seedWELLRNG512a, generateWELLRNG512a);
}

void putRngWELL521(unsigned int *state)
{
	InitWELLRNG521a( state );
	user_unif_set_generator(2, seedWELLRNG521a, generateWELLRNG521a);
}

void putRngWELL607(unsigned int *state)
{
	InitWELLRNG607a( state );
	user_unif_set_generator(2, seedWELLRNG607a, generateWELLRNG607a);
}

void putRngWELL800(unsigned int *state)
{
	InitWELLRNG800a( state );
	user_unif_set_generator(2, seedWELLRNG800a, generateWELLRNG800a);
}

void putRngWELL1024(unsigned int *state)
{
	InitWELLRNG1024a( state );
	user_unif_set_generator(2, seedWELLRNG1024a, generateWELLRNG1024a);
}

void putRngWELL19937(unsigned int *state)
{
	InitWELLRNG19937a( state );
	user_unif_set_generator(2, seedWELLRNG19937a, generateWELLRNG19937a);
}

void putRngWELL21701(unsigned int *state)
{
	InitWELLRNG21701a( state );
	user_unif_set_generator(2, seedWELLRNG21701a, generateWELLRNG21701a);
}

void putRngWELL23209(unsigned int *state)
{
	InitWELLRNG23209a( state );
	user_unif_set_generator(2, seedWELLRNG23209a, generateWELLRNG23209a);
}

void putRngWELL44497(unsigned int *state)
{
	InitWELLRNG44497a( state );
	user_unif_set_generator(2, seedWELLRNG44497a, generateWELLRNG44497a);
}

void putRngWELL(int *porder, int *pversion, int *ptemp, unsigned int *state)
{
	order = *porder;
	version = *pversion;
	temp = *ptemp;
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
	}
}

// get state functions

void getRngWELL512(unsigned int *state)
{
	GetWELLRNG512a( state );
}

void getRngWELL521(unsigned int *state)
{
	GetWELLRNG521a( state );
}

void getRngWELL607(unsigned int *state)
{
	GetWELLRNG607a( state );
}

void getRngWELL800(unsigned int *state)
{
	GetWELLRNG800a( state );
}

void getRngWELL1024(unsigned int *state)
{
	GetWELLRNG1024a( state );
}

void getRngWELL19937(unsigned int *state)
{
	GetWELLRNG19937a( state );
}

void getRngWELL21701(unsigned int *state)
{
	GetWELLRNG21701a( state );
}

void getRngWELL23209(unsigned int *state)
{
	GetWELLRNG23209a( state );
}

void getRngWELL44497(unsigned int *state)
{
	GetWELLRNG44497a( state );
}

void getRngWELL(int *porder, int *pversion, int *ptemp, unsigned int *state)
{
	*porder = order;
	*pversion = version;
	*ptemp = temp;
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
	}
}

