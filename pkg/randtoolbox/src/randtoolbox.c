/** 
 * @file  randtoolbox.c
 * @brief C file for all RNGs
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2019, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * Christophe Dutang, see http://dutangc.free.fr 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2019 Christophe Dutang, Petr Savicky. 
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
 *  various Random Number Generators
 *
 *		C functions
 *  
 *	Many ideas are taken from <Rsource>/src/main/RNG.c
 *
 */

#include "randtoolbox.h"

/*********************************/
/*              constants               */
//the seed
static unsigned long seed; 
//static unsigned long torusoffset;
//a pseudo boolean to initiate the seed
static int isInit=0;
//the length (maximal) of the internal seed array for WELL44497
#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
//a pseudo boolean to initiate the seed array
static int isInitByArray=0;

//the first 100 000 prime numbers computed from their differences stored in primes.h
static int primeNumber[100000];

// pi
const long double constpi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 ;
const long double two_64_d = 18446744073709551616.0;
const uint64_t two_64m1_h = 0xffffffffffffffff;

/*********************************/
/*          utility functions         */

//fractional part
static R_INLINE double fracPart(double x)
{
  return x - floor(x);
}


/*********************************/
/* quasi random generation */

//main function used .Call()
SEXP doTorus(SEXP n, SEXP d, SEXP p, SEXP offset, SEXP ismixed, SEXP timedseed, SEXP mersexpo)
{
  if (!isNumeric(n) || !isNumeric(d) || !isLogical(timedseed) || !isNumeric(mersexpo))
    error(_("invalid argument"));
  
  if(!isNull(p) && !isNumeric(p))           
    error(_("invalid argument"));
  
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int *prime; //prime numbers used when supplied
  int seqstart = asInteger( offset ); //sequence starting point
  int mixed = asLogical( ismixed ); //boolean to use the mixed Torus algo
  int usetime = asLogical( timedseed ); //boolean to use the machine time
  int mexp = asInteger( mersexpo );  //mersenne exponent
  
  if( isNull(p) )
    prime = NULL; 
  else 
    prime  = INTEGER( p ); 
  
  
  //allocate result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  if(prime == NULL)
  {
    if (primeNumber[2] == 1)
      reconstruct_primes();
    torus(u, nb, dim, primeNumber, seqstart, mixed, usetime, mexp);
  } else
    torus(u, nb, dim, prime, seqstart, mixed, usetime, mexp);
  
  UNPROTECT(1);
  
  return resultinR;
}

//compute the vector sequence of the Torus algorithm
//pure QMC : offset is used ; ismixed=usetime=0
//QMC with time machine : seed is used ; ismixed=0, usetime=1
//hybrid QMC with SFMT : seed is used ; ismixed=1
void torus(double *u, int nb, int dim, int *prime, int offset, int ismixed, int usetime, int mexp)
{
  int i, j;
  unsigned long state;
  
  if (!R_FINITE(nb) || !R_FINITE(dim))
    error(_("non finite argument"));
  
  if(prime == NULL)    
    error(_("internal error in torus function"));
  
  //sanity check
  if(dim > 100000) 
    error(_("Torus algorithm not yet implemented for dimension %d"), dim);
  
  
  //u_ij is the Torus sequence term 
  //with n = state + i, s = j + 1, p = primeNumber[j] or prime[j]
  //u is stored column by column
  
  if(ismixed) //SF Mersenne-Twister-mixed Torus algo
  { 
    //init SFMT parameters
    init_SFMT(mexp, 0); //a single set of parameters
    //init the seed of SF Mersenne Twister algo
    SFMT_init_gen_rand(seed);
    
    //compute
    for(j = 0; j < dim; j++)
    {    
      for(i = 0; i < nb; i++) 
      {
        state = SFMT_gen_rand32();
        u[i + j * nb] = fracPart( state * sqrt( prime[j] ) ) ;
      }
    }
  }else //pure Torus algo
  {
    if(usetime) //use the machine time
    {
      //init the state of Torus algo (only for pure QMC and usetime=1)
      if(!isInit && usetime) 
        randSeed();
      state = ((unsigned int) seed >> 16);
    }else 
      state  = offset;
    //compute
    for(j = 0; j < dim; j++)
      for(i = 0; i < nb; i++) 					
        u[i + j * nb] = fracPart( ( state + i ) * sqrt( prime[j] ) ) ;                
  }
  
  isInit = 0;
}

SEXP doHalton(SEXP n, SEXP d, SEXP offset, SEXP ismixed, SEXP timedseed, SEXP mersexpo)
{
  if (!isNumeric(n) || !isNumeric(d) || !isLogical(timedseed) || !isNumeric(mersexpo) )
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int seqstart = asInteger( offset ); //sequence starting point
  int mixed = asLogical( ismixed ); //boolean to use the mixed Halton algo
  int usetime = asLogical( timedseed ); //boolean to use the machine time
  int mexp = asInteger( mersexpo );  //mersenne exponent
  
  //allocate result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  if (primeNumber[2] == 1)
    reconstruct_primes();
  
  halton_c(u, nb, dim, seqstart, mixed, usetime, mexp);
  
  UNPROTECT(1);
  
  return resultinR;
}


//compute the vector sequence of the Halton algorithm
void halton_c(double *u, int nb, int dim, int offset, int ismixed, int usetime, int mexp)
{
  int i, j;
  uint32_t state;
  
  if (!R_FINITE(nb) || !R_FINITE(dim))
    error(_("non finite argument"));
  
  //sanity check
  if(dim > 100000)
    error(_("Halton algorithm not yet implemented for dimension %d"), dim);
  
  //u_ij is the Halton sequence term
  //with n = state + i, s = j + 1, p = primeNumber[j]
  //u is stored column by column
  
  if(ismixed) //SF Mersenne-Twister-mixed Torus algo
  {
    //init SFMT parameters
    init_SFMT(mexp, 0); //a single set of parameters
    //init the seed of SFMT
    SFMT_init_gen_rand(seed);
    //compute
    for(j = 0; j < dim; j++)
    {
      for(i = 0; i < nb; i++)
      {
        state = SFMT_gen_rand32();
        u[i + j * nb] = HALTONREC( j, state ) ;
      }
    }
  }else //classic Halton algo
  {
    if(usetime) //use the machine time
    {
      //init the seed of Halton algo
      if(!isInit) 
        randSeed(); // (only for pure QMC and usetime=1)
      state = ((unsigned int) seed >> 16);
    }else
      state  = offset;
    
    for(j = 0; j < dim; j++)
      for(i = 0; i < nb; i++)
        u[i + j * nb] = HALTONREC( j, state + i ) ;
  }
  
  isInit = 0;
}



SEXP doSobol(SEXP n, SEXP d, SEXP offset, SEXP ismixed, SEXP timedseed, SEXP mersexpo)
{
  if (!isNumeric(n) || !isNumeric(d) || !isLogical(timedseed) || !isNumeric(mersexpo))
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int seqstart = asInteger( offset ); //sequence starting point
  int mixed = asLogical( ismixed ); //boolean to use the mixed Halton algo
  int usetime = asLogical( timedseed ); //boolean to use the machine time
  int mexp = asInteger( mersexpo );  //mersenne exponent
  
  //allocate result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  if (primeNumber[2] == 1)
    reconstruct_primes();
  
  sobol_c(u, nb, dim, seqstart, mixed, usetime, mexp);
  
  UNPROTECT(1);
  
  return resultinR;
}



//compute the vector sequence of the Sobol algorithm
void sobol_c(double *u, int nb, int dim, int offset, int ismixed, int usetime, int mexp)
{
  //temporary working variables
  int i, j;
  int ll;
  //unsigned long state;
  
  int *sv; //possibly scrambled direction numbers
  int maxbit=30; //maximum number of bits for direction numbers
  //allocate temporary variables
  sv = (int *) R_alloc(maxbit*dim, sizeof(int));
  
  
  if (!R_FINITE(nb) || !R_FINITE(dim))
    error(_("non finite argument"));
  
  //sanity check
  if(dim > 1111)
    error(_("Sobol algorithm not yet implemented for dimension %d"), dim);
  
  //init the seed [TO CHECK]
  if(!isInit)
    randSeed();
  
  INITSOBOL(dim, u, &ll, nb, sv, 0, seed);
  
  for(j = 0; j < dim; j++)
  {
    Rprintf("Direction %u\n", j);
    for(i = 0; i < maxbit; i++)
      Rprintf("%u,", sv[i + j * maxbit]);
    Rprintf("\n");
  }
  
  //u_ij is the Sobol sequence term
  //with n = state + i, s = j + 1, p = primeNumber[j]
  //u is stored column by column
  
  for(j = 0; j < dim; j++)
    for(i = 0; i < nb; i++)
      u[i + j * nb] = 0.0;
  
  isInit = 0;
}



/***********************************/
/* pseudo random generation */ 

//main function used .Call()
SEXP doCongruRand(SEXP n, SEXP d, SEXP modulus, SEXP multiplier, SEXP increment, SEXP echo)
{
  if (!isNumeric(n) || !isNumeric(d))
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int show =  asLogical( echo ); //to show the seed
  double modultemp = asReal( modulus ) ; //modulus as a double numeric
  double multtemp = asReal( multiplier ) ; //multiplier as a double numeric
  double incrtemp = asReal( increment ) ; //increment as a double numeric
  uint64_t mod, mult, incr, mask; //modulus, multiplier, increment, mask
  
  //define a mask as in congruRand.c (function put_state_congru())
  if (modultemp < two_64_d) //modulus lesser than 2^64 => mask=2^d-1 if mod = 2^d
  {  
    mod = (uint64_t) modultemp; //modulus below 2^64
    if ((mod & (mod-1)) == 0)
      mask = mod-1;
    else
      mask = 0;
  }else //modulus greater or equal than 2^64 => mask=2^64-1 and mod=0 
  {
    mod = 0;
    mask = two_64m1_h; 
  }
  if (multtemp < two_64_d) 
    mult = (uint64_t) multtemp; //multiplier below 2^64
  else
    error(_("multiplier greater than 2^64-1"));
  if (incrtemp < two_64_d) 
    incr = (uint64_t) incrtemp; //increment below 2^64  
  else
    error(_("increment greater than 2^64-1"));
  
  //result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  congruRand(u, nb, dim, mod, mult, incr, mask, show);
  
  UNPROTECT(1);
  
  return resultinR;
}

//compute the sequence of a general congruential linear generator
void congruRand(double *u, int nb, int dim, uint64_t mod, uint64_t mult, uint64_t incr, uint64_t mask, int show)
{
  int i, j, err;
  uint64_t temp;
  
  if (!R_FINITE(nb) || !R_FINITE(dim))
    error(_("non finite argument"));
  
  //initiate the seed with the machine time
  // and ensure it is positive
  if(!isInit) 
  {    
    do randSeed() ; 
    while ( seed <= 0 );
  }
  
  //u_ij is the nth (n = i + j * nb) term of a general congruential linear generator
  //i.e. u_ij = [ ( mult * x_{n-1}  + incr ) % mod ] / mod
  //u is stored column by column
  if (mod > 0) 
    seed = seed % mod;
  err = check_congruRand(mod, mask, mult, incr, (uint64_t) seed);
  if (err < 0)
    error(_("incorrect parameters of the generator %d"), err);
  set_congruRand(mod, mult, incr, (uint64_t) seed, mask);
  
  if(!show) 
  {
    for(i = 0; i < nb; i++)
    {
      for(j = 0; j < dim; j++) 
      {
        u[i + j * nb] = get_congruRand();
      }
    }
  }else //show the seed
  {    
    for(i = 0; i < nb; i++)
    {
      for(j = 0; j < dim; j++) 
      {
        get_seed_congruRand((uint64_t*) &temp);
        Rprintf("%u th integer generated : %" PRIu64 "\n", 1+ i + j * nb, temp);
        u[i + j * nb] = get_congruRand();
      }
    }
  }
  
  isInit = 0;
}

//main function used .Call()
SEXP doSFMersenneTwister(SEXP n, SEXP d, SEXP mersexpo, SEXP paramset)
{
  if (!isNumeric(n) || !isNumeric(d) || !isNumeric(mersexpo) || !isLogical(paramset))
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int mexp = asInteger( mersexpo );  //mersenne exponent
  int usepset = asLogical( paramset ); //use param sets
  
  //result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  SFmersennetwister(u, nb, dim, mexp, usepset );
  
  UNPROTECT(1);
  return resultinR;   
}

// call the SF mersenne twister of Matsumoto and Saito
void SFmersennetwister(double *u, int nb, int dim, int mexp, int usepset)
{
  int i, j;
  
  //initiate the seed with the machine time
  // and ensure it is positive
  if(!isInit) 
  {    
    do randSeed() ; 
    while ( seed <= 0 );
  }
  
  //init SFMT parameters
  init_SFMT(mexp, usepset);
  //init the seed of SFMT
  SFMT_init_gen_rand(seed);
  
#if defined(HAVE_SSE2)	
  //size of internal array
  int blocksize = get_min_array_size32();
#endif
  
  //number of blocks to generate
  //int nbblock = nb / blocksize; 
  //last variates to generate
  //int rest = nb % blocksize;
  
  
  /*
   * @param size the number of 32-bit pseudorandom integers to be
   * generated.  size must be a multiple of 4, and greater than or equal
   * to (MEXP / 128 + 1) * 4.
   */
#if defined(HAVE_SSE2)    
  if(nb * dim >= blocksize)
  {
    __m128i array1[nb * dim*4];
    
    uint32_t *array32 = (uint32_t *)array1;
    
    fill_array32(array32, nb*dim);
    
    // compute u_ij
    for(j = 0; j < dim; j++)
      for(i = 0; i < nb; i++) 
        u[i + j * nb] = to_real3( array32[i + j * nb] ); // real on ]0,1[ interval
    
  }
  else
  {
#endif        
    
    // compute u_ij
    for(j = 0; j < dim; j++)
      for(i = 0; i < nb; i++) 
        u[i + j * nb] = SFMT_genrand_real3(); // real on ]0,1[ interval
    
#if defined(HAVE_SSE2)
  }
#endif
  
  
  isInit = 0;
}

//main function used .Call()
SEXP doWELL(SEXP n, SEXP d, SEXP order, SEXP tempering, SEXP version)
{
  if (!isNumeric(n) || !isNumeric(d) || !isNumeric(order) || !isLogical(tempering) || !isNumeric(version))
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  int degree = asInteger( order );  //mersenne exponent
  int dotemper = asLogical( tempering ); //tempering or not?
  int theversion = asInteger( version ); //1 for 'a' version and 2 for 'b'
  
  //result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  WELLrng(u, nb, dim, degree, dotemper, theversion);
  
  UNPROTECT(1);
  
  return resultinR;   
}



//main function used .Call()
SEXP doKnuthTAOCP(SEXP n, SEXP d)
{
  if (!isNumeric(n) || !isNumeric(d))
    error(_("invalid argument"));
  
  //temporary working variables
  int nb = asInteger( n ); //number of random vectors
  int dim  = asInteger( d ); //dimension of vector
  
  //result
  double *u ; //result in C
  SEXP resultinR; //result in R
  PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
  u = REAL( resultinR ); //plug the C pointer on the R type
  
  R_CheckStack();
  
  //computation step
  knuthTAOCP(u, nb, dim);
  
  UNPROTECT(1);
  
  return resultinR;   
}

// call the Knuth 'The Art Of Computer Programming' RNG
void knuthTAOCP(double *u, int nb, int dim)
{
  int i, j;
  
  //initiate the seed with the machine time
  // and ensure it is positive
  if(!isInit) 
  {    
    do randSeed() ; 
    while ( seed <= 0 );
  }
  
  //init TAOCP RNG
  ranf_start( seed );
  
  // compute u_ij's
  // declare an array a little bit longer than KK (100) long lag if too short
  // see Knuth's file for details
  if( nb * dim <= 100 )
  {
    double * temp = (double *) R_alloc( 101, sizeof(double) );
    
    ranf_array( temp, 101 );
    
    for(j = 0; j < dim; j++)
      for(i = 0; i < nb; i++) 
        u[i + j * nb] = temp[i + j * nb]; // real on ]0,1[ interval
    
  }
  else
    ranf_array( u, nb*dim );

  isInit = 0;
}


/**********************************/
/*          set the seed                */

//main function used .Call()
//seed set by the user
//idea taken from the R internal C function do_setseed
SEXP doSetSeed(SEXP s)
{
  if (!isNumeric(s))
    error(_("invalid argument"));
  
  setSeed( (long) asInteger(s) );
  
  return R_NilValue;
}

void setSeed(long s)
{
  if (!R_FINITE(s))
    error(_("non finite seed"));
  
  seed = s;
  isInit = 1;
  isInitByArray = 0;
}

//randomize and set the seed when not initialized
//idea taken from the R internal C function Randomize()
void randSeed()
{
  
#if HAVE_SYS_TIME_H
{
  /* 
   * UTC time since the Epoch, i.e. 01/01/1970 00:00:00
   struct timeval {  
   unsigned long tv_sec; // seconds 
   long tv_usec; // and microseconds  }; 
   * see http://opengroup.org/onlinepubs/007908799/xsh/systime.h.html
   */
  
  struct timeval tv;
  gettimeofday (&tv, NULL);
  
  seed = ((unsigned long long) tv.tv_usec << 16) ^ tv.tv_sec;
}
#elif HAVE_WINDOWS_H
{
  /* 
   * UTC time since the Epoch, i.e. 01/01/1970 00:00:00
   typedef struct _SYSTEMTIME {
   WORD wYear;
   WORD wMonth;
   WORD wDayOfWeek;
   WORD wDay;
   WORD wHour;
   WORD wMinute;
   WORD wSecond;
   WORD wMilliseconds;
   } SYSTEMTIME, 
   *PSYSTEMTIME;
   * see http://msdn.microsoft.com/en-us/library/ms724950(VS.85).aspx
   */
  
  SYSTEMTIME tv;
  GetSystemTime(&tv);
  
  /*
   *  typedef union _LARGE_INTEGER {
   struct {
   DWORD LowPart;
   LONG HighPart;
   } ;
   struct {
   DWORD LowPart;
   LONG HighPart;
   } u;
   LONGLONG QuadPart;
   } LARGE_INTEGER, 
   *PLARGE_INTEGER;
   * see http://msdn.microsoft.com/en-us/library/aa383713(VS.85).aspx 
   */
  
  LARGE_INTEGER count;
  QueryPerformanceCounter( (LARGE_INTEGER *) &count );        
  
  seed = (unsigned long long) ( ( (tv.wMilliseconds << 16) ^ tv.wSecond ) + count.LowPart );
}
#elif HAVE_TIME_H
{
  /* 
   * UTC time since the Epoch, i.e. 01/01/1970 00:00:00
   type time_t  
   tv_sec    seconds
   * see http://opengroup.org/onlinepubs/007908799/xsh/time.h.html
   */
  
  seed = ((unsigned long) time(NULL) <<16);
}
#else
/* unlikely, but use random contents */
#endif

isInit = 1;
}   

//initialize internal state array, idea taken from Matsumoto's code dSFMT
void randSeedByArray(int length)
{
  int i;
  //unsigned long long int temp = 1;
  
  if( length > LENSEEDARRAY)
    error(_("error while initializing WELL generator\n"));
  
  if (!isInit) randSeed();
  
  // same initialisation as dSFMT 1.3.0 from Matsumoto and Saito
  seedArray[0] = seed;
  for (i = 1; i < length; i++) 
    seedArray[i] = 1812433253UL * ( seedArray[i - 1] ^ ( seedArray[i - 1] >> 30 ) ) + i;
  
  isInit = 0;
  isInitByArray = 1;
}



/**************/
/* constants */
//the first 100 000 prime numbers taken from http://primes.utm.edu/
#include "primes.h"

void reconstruct_primes()
{
  int i;
  if (primeNumber[2] == 1)
    for (i = 2; i < 100000; i++)
      primeNumber[i] = primeNumber[i-1] + 2*primeNumber[i];
}

void get_primes(int *n, int *pri)
{
  int i;
  if (primeNumber[2] == 1)
    reconstruct_primes();
  for (i = 0; i < *n; i++)
    pri[i] = primeNumber[i];
}

