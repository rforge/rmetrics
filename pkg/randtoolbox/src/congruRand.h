/* License information to be included */

//R header files
#include <R.h>
#include <Rmath.h>


double user_unif_rand_congru();
void user_unif_init_congru(unsigned int seed);

double get_congruRand();
int check_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed);
void set_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed);
void get_seed_congruRand(unsigned long long *out_seed);
void get_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);
void put_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);

