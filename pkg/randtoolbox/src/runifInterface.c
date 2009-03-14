/* License information to be included */

#include <R.h>
#include <Rmath.h>
#include <R_ext/Random.h>

#include "congruRand.h"
#include "runifInterface.h"

static int generator;
static double (*user_unif_rand_selected) (void); // not (double *) as user_unif_rand
static void (*user_unif_init_selected) (unsigned int seed);
double x;

// R_ext/Random.h entry point
double *user_unif_rand(void)
{
    x = user_unif_rand_selected();
    return(&x);
}

// R_ext/Random.h entry point
void user_unif_init(unsigned int seed)
{
	seed = 3602842457U * seed + 105890386U; // undo initial scrambling
    user_unif_init_selected(seed);
}

// .C entry point
void current_generator(int *pgener)
{
	*pgener = generator;
}

// .C entry point
void set_user_unif_init(int *pgener)
{
	generator = *pgener;
	switch (generator) {
		case 1:
			user_unif_init_selected = user_unif_init_congru;
			break;
		default:
			Rprintf("UNKNOWN GENERATOR\n");
	}
}

// .C entry point
void set_user_unif_rand(int *pgener)
{
	if (generator != *pgener) {
		Rprintf("INTERNAL ERROR of randtoolbox\n");
	}
	switch (generator) {
		case 1:
			user_unif_rand_selected = user_unif_rand_congru;
			break;
		default:
			Rprintf("UNKNOWN GENERATOR\n");
	}
}

