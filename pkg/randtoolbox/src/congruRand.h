/* License information to be included */

double get_rand_congru();
double get_congruRand();
void set_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned int seed);
void user_unif_init_congru(unsigned int seed);
void get_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);
void put_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);

