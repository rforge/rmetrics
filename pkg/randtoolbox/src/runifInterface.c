/* License information to be included */

#include <R.h>
#include <Rmath.h>
#include <R_ext/Random.h>

#include "congruRand.h"

static int generator;
static double (*get_rand) (void);
static void (*user_unif_init_selected) (unsigned int seed);
double x;

// R_ext/Random.h entry point
double *user_unif_rand(void)
{
    x = get_rand();
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
void set_generator(int *pgener)
{
	generator = *pgener;
	switch (generator) {
		case 1:
			user_unif_init_selected = user_unif_init_congru;
			get_rand = get_rand_congru;
			break;
		default:
			Rprintf("UNKNOWN GENERATOR\n");
	}
}

