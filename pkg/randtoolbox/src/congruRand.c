/* License information to be included */

#include <R.h>
#include <Rmath.h>

#include "congruRand.h"

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

// .C entry point
void get_state_congru(double *pmod, double *pmult, double *pincr, double *pseed)
{
	*pmod = (double) mod;
	*pmult = (double) mult;
	*pincr = (double) incr;
	*pseed = (double) congru_seed;
}

// .C entry point
void check_state_congru(double *pmod, double *pmult, double *pincr, double *pseed, int *err)
{
	unsigned long long inp_mod, inp_mult, inp_incr, inp_seed;
	inp_mod = (unsigned long long) *pmod;
	inp_mult = (unsigned long long) *pmult;
	inp_incr = (unsigned long long) *pincr;
	inp_seed = (unsigned long long) *pseed;
	*err = check_congruRand(inp_mod, inp_mult, inp_incr, inp_seed);
}

// .C entry point
void put_state_congru(double *pmod, double *pmult, double *pincr, double *pseed)
{
	unsigned long long inp_mod, inp_mult, inp_incr, inp_seed;
	inp_mod = (unsigned long long) *pmod;
	inp_mult = (unsigned long long) *pmult;
	inp_incr = (unsigned long long) *pincr;
	inp_seed = (unsigned long long) *pseed;
	set_congruRand(inp_mod, inp_mult, inp_incr, inp_seed);
}

