#include <R.h>
#include "WELL19937a.h"

#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];

void (*user_unif_set_generator)(int gener, void * selected_init, void * selected_rand);

void WELL_get_set_entry_point(void * p_user_unif_set_generator)
{
	user_unif_set_generator = (void (*)(int, void *, void *)) p_user_unif_set_generator;
}

void initWELLRNG19937a(unsigned int seed)
{
	int i;
	seedArray[0] = seed;
	for (i = 1; i < 624; i++)
		seedArray[i] = 1812433253UL * ( seedArray[i - 1] ^ ( seedArray[i - 1] >> 30 ) ) + i;
	InitWELLRNG19937a( seedArray );
}

double generateWELLRNG19937a()
{
	return WELLRNG19937a();
}

void setRngWELL()
{
	user_unif_set_generator(2, initWELLRNG19937a, generateWELLRNG19937a);
}

