#include "R.h"
#include <R_ext/Random.h>

#include "WELL512a.h"
#include "WELL1024a.h"
#include "WELL19937a.h"
#include "WELL19937c.h"
#include "WELL44497a.h"
#include "WELL44497b.h"

#define m1   4294949027.0
#define m2   4294934327.0
extern double  s10, s11, s12, s13, s14, s20, s21, s22, s23, s24;
unsigned int MRG32k5a();

static void (*InitWELLSelected)();

#define SizeOfState 1391

unsigned int prepare[SizeOfState];

double x;

double getWELLRNG512a()
{
	x = WELLRNG512a();
}

double getWELLRNG1024a()
{
	x = WELLRNG1024a();
}

double getWELLRNG19937a()
{
	x = WELLRNG19937a();
}

double getWELLRNG19937c()
{
	x = WELLRNG19937c();
}

double getWELLRNG44497a()
{
	x = WELLRNG44497a();
}

double getWELLRNG44497b()
{
	x = WELLRNG44497b();
}

static double (*user_unif_rand_selected)();

// R_ext/Random.h entry point
double *user_unif_rand(void)
{
	user_unif_rand_selected();
	return(&x);
}

void init_mrg32k5a(int n, unsigned int *seed)
{
	int i,j;
	unsigned int l16, h16;
	s10 = 12345.0;
	s11 = 12345.0;
	s12 = 12345.0;
	s13 = 12345.0;
	s14 = 12345.0;
	s20 = 12345.0;
	s21 = 12345.0;
	s22 = 12345.0;
	s23 = 12345.0;
	s24 = 12345.0;
	for (i=0; i < n; i++) {
		l16 = seed[i] & 0x0000ffff;
		h16 = seed[i] >> 16;
		s14 += (double) h16 + 1.0;
		s24 += (double) h16 + 1.0;
		if (s14 >= m1) { s14 -= m1; }
		if (s24 >= m2) { s24 -= m2; }
		for (j=0; j<5; j++) MRG32k5a();
		s14 += (double) l16 + 1.0;
		s24 += (double) l16 + 1.0;
		if (s14 >= m1) { s14 -= m1; }
		if (s24 >= m2) { s24 -= m2; }
		for (j=0; j<5; j++) MRG32k5a();
	}
	for (i = 0; i < SizeOfState; i++) {
		prepare[i] = (MRG32k5a() & 0x0000ffff);
		prepare[i] = (prepare[i] << 16) | (MRG32k5a() & 0x0000ffff);
	}
}

//void init_vector_mrg32k5a(int *n, double *seed, unsigned int *iseed, unsigned int *state)
//{
//	int i;
//	for (i=0; i < *n; i++) {
//		iseed[i] = (unsigned int) seed[i];
//	}
//	init_mrg32k5a(*n, iseed);
//	for (i=0; i < 625; i++) {
//		state[i] = random_seed[i];
//	}
//}

void user_unif_init_mrg32k5a(unsigned int seed)
{
	init_mrg32k5a(1, &seed);
	InitWELLSelected(prepare);
}

void user_unif_init_sfmt(unsigned int seed)
{
	int i;
	prepare[0] = seed;
	for (i = 1; i < SizeOfState; i++) 
		prepare[i] = 1812433253UL * ( prepare[i - 1] ^ ( prepare[i - 1] >> 30 ) ) + i;
	InitWELLSelected(prepare);
}

// this is called by RNGkind("user-supplied")
void no_operation(unsigned int seed)
{
    ;
}

static void (*user_unif_init_selected) (unsigned int seed);

// .C entry point
void set_noop(void)
{
    user_unif_init_selected = no_operation;
}

// R_ext/Random.h entry point
void user_unif_init(unsigned int seed)
{
	seed = 3602842457U * seed + 105890386U; // undo initial scrambling
	user_unif_init_selected(seed);
}

// .C entry point
void set_generator(int *order, int *version, int *initialization)
{
	switch (100*(*order) + (*version)) {
	case 51201: 
		user_unif_rand_selected = getWELLRNG512a;
		InitWELLSelected = InitWELLRNG512a;
		break;
	case 102401: 
		user_unif_rand_selected = getWELLRNG1024a;
		InitWELLSelected = InitWELLRNG1024a;
		break;
	case 1993701: 
		user_unif_rand_selected = getWELLRNG19937a;
		InitWELLSelected = InitWELLRNG19937a;
		break;
	case 1993703: 
		user_unif_rand_selected = getWELLRNG19937c;
		InitWELLSelected = InitWELLRNG19937c;
		break;
	case 4449701: 
		user_unif_rand_selected = getWELLRNG44497a;
		InitWELLSelected = InitWELLRNG44497a;
		break;
	case 4449702: 
		user_unif_rand_selected = getWELLRNG44497b;
		InitWELLSelected = InitWELLRNG44497b;
		break;
	default:
		printf("order %d, version %d unsupported\n", *order, *version);
	}
	switch (*initialization) {
	case 1:
		user_unif_init_selected = user_unif_init_mrg32k5a;
		break;
	case 2:
		user_unif_init_selected = user_unif_init_sfmt;
		break;
	default:
		printf("initialization %d unsupported\n", *initialization);
	}
}

