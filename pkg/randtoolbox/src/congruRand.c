/* License information to be included */

#include <R.h>
#include <Rmath.h>

// general linear congruential generator

unsigned long long mod, mult, incr, congru_seed;

double get_rand_congru()
{
	double x;
	congru_seed  = (mult * congru_seed + incr) % mod;
	x = (double) congru_seed / (double) mod;
	if (x == 0.0) {
		x = 0.5 / (double) mod;
	}
	return x;
}

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

void set_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned int seed)
{
	mod = inp_mod;
	mult = inp_mult;
	incr = inp_incr;
	congru_seed = (unsigned long long) seed;
}

void user_unif_init_congru(unsigned int seed)
{
	congru_seed = seed;
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
void put_state_congru(double *pmod, double *pmult, double *pincr, double *pseed)
{
	mod = (unsigned long long) *pmod;
	mult = (unsigned long long) *pmult;
	incr = (unsigned long long) *pincr;
	congru_seed = (unsigned long long) *pseed;
}

