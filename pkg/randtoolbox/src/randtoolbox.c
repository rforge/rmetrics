/** 
 * @file  randtoolbox.c
 * @brief C file for all RNGs
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2009, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2009 Christophe Dutang, Petr Savicky. 
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
static unsigned long torusoffset;
//a pseudo boolean to initiate the seed
static int isInit=0;
//the length (maximal) of the internal seed array for WELL44497
#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
//a pseudo boolean to initiate the seed array
static int isInitByArray=0;

//the first 100 000 prime numbers taken from http://primes.utm.edu/ included at the end of the file
static int primeNumber[100000];

// pi
const long double constpi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 ;


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
SEXP doTorus(SEXP n, SEXP d, SEXP p, SEXP offset, SEXP ismixed, SEXP timedseed)
{
    if (!isNumeric(n) || !isNumeric(d) || !isLogical(timedseed) )
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
    if(prime == NULL) {
        if (primeNumber[2] == 1)
            reconstruct_primes();
        torus(u, nb, dim, primeNumber, seqstart, mixed, usetime);
    } else
        torus(u, nb, dim, prime, seqstart, mixed, usetime);
	
    UNPROTECT(1);
	
    return resultinR;
}

//compute the vector sequence of the Torus algorithm
void torus(double *u, int nb, int dim, int *prime, int offset, int ismixed, int usetime)
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
    
    //init the seed of Torus algo
    if(!isInit) 
        randSeed();	
    
    //init the state of SF Mersenne Twister algo
    if(ismixed)        
        init_gen_rand(seed);
    
    
    //u_ij is the Torus sequence term 
    //with n = state + i, s = j + 1, p = primeNumber[j] or prime[j]
    //u is stored column by column
    
    if(ismixed) //SF Mersenne-Twister-mixed Torus algo
    { 
        for(j = 0; j < dim; j++)
        {    
            for(i = 0; i < nb; i++) 
            {
                state = gen_rand32();
                u[i + j * nb] = fracPart( state * sqrt( prime[j] ) ) ;
            }
        }
    }
    else //classic Torus algo
    {
        if(usetime) //use the machine time
            state = seed;
        else 
            state  = offset;
        
        for(j = 0; j < dim; j++)
            for(i = 0; i < nb; i++) 
                u[i + j * nb] = fracPart( ( state + i ) * sqrt( prime[j] ) ) ;                
    }
    
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
    
    unsigned long long mod = asInteger( modulus ); //modulus
    unsigned long long mult = asInteger( multiplier ); //modulus
    unsigned long long incr = asInteger( increment ); //modulus    
	
    //result
    double *u ; //result in C
    SEXP resultinR; //result in R
    PROTECT(resultinR = allocMatrix(REALSXP, nb, dim)); //allocate a n x d matrix
    u = REAL( resultinR ); //plug the C pointer on the R type
	
    R_CheckStack();
	
    //computation step
    congruRand(u, nb, dim, mod, mult, incr, show);
	
    UNPROTECT(1);
	
    return resultinR;
}

//compute the sequence of a general congruential linear generator
void congruRand(double *u, int nb, int dim, unsigned long long mod, unsigned long long mult, unsigned long long incr, int show)
{
    int i, j, err;
    unsigned long long temp;
	
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
    if (mod > 0) seed = seed % mod;
    err = check_congruRand(mod, mult, incr, (unsigned long long) seed);
    if (err)
        error(_("incorrect parameters of the generator %u"), err);
    set_congruRand(mod, mult, incr, (unsigned long long) seed);

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
                get_seed_congruRand(&temp);
                Rprintf("%u th integer generated : %llu\n", 1+ i + j * nb, temp);
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
    init_gen_rand(seed);
    
    //size of internal array
    int blocksize = get_min_array_size32();
    //number of blocks to generate
    //int nbblock = nb / blocksize; 
    //last variates to generate
    //int rest = nb % blocksize;
    
    
    
    //Rprintf("zog\n");
    
//    PROTECT(array = allocMatrix(INTSXP, nb, dim)); //allocate a n x d matrix
    
   
    /*
    * @param size the number of 32-bit pseudorandom integers to be
    * generated.  size must be a multiple of 4, and greater than or equal
    * to (MEXP / 128 + 1) * 4.
    */
#if defined(HAVE_SSE2)    
    if(nb * dim >= blocksize)
    {

        //unsigned int *array;
//        static __m128i array1[BLOCK_SIZE / 4];
//        static __m128i array2[10000 / 4];

        __m128i array1[nb * dim*4];

        uint32_t *array32 = (uint32_t *)array1;
        
//        array = (unsigned int *) R_alloc(nb * dim, sizeof(__m128i));
        
        //Rprintf("blocksize %d\n", blocksize);
        
        //Rprintf("mem %d \n", array);
        
        //int roundedsize = ( (nb*dim) / 4 ) *4;
        //Rprintf("rounded size %d initial size %d\n", roundedsize, nb*dim);
        fill_array32(array32, nb*dim);
        //Rprintf("fill_array32 ends\n");
    
//    for(j = 0; j < dim; j++)
//        fill_array32( (array+ i*blocksize), rest);
//    fill_array32( (array + i + j * nbblock), rest);
//
//    for(j = 0; j < dim; j++)
 //   {
  //      for(i = 0; i < nb; i++) 
   //     {
    //        Rprintf("%u \t", array[i + j * nb] ); // real on ]0,1[ interval
     //   }
   // Rprintf("\n");
//}
    
    
    // compute u_ij
    for(j = 0; j < dim; j++)
        for(i = 0; i < nb; i++) 
            u[i + j * nb] = to_real3( array32[i + j * nb] ); // real on ]0,1[ interval
            
        
    //Free(array);
    }
    else
    {
#endif        


    // compute u_ij
    for(j = 0; j < dim; j++)
        for(i = 0; i < nb; i++) 
            u[i + j * nb] = genrand_real3(); // real on ]0,1[ interval

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
    
//    Rprintf("call wellrng\n");
    
    //computation step
    WELLrng(u, nb, dim, degree, dotemper, theversion);
    
  //  Rprintf("fin WELLrng\n");
    
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
        
    //ranf_arr_cycle();
    
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
    

    
    //Rprintf("1st term %.20f --- seed  %u\n", u[0], seed);
        
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

//    Rprintf("length %d \n", length);
  /*  for(i = 0; i < length/2; i++)
    {
        seedArray[i] = ( (unsigned long long) seed << (i+1) ) ^ ( (unsigned long long) seed >> (i+1) );
        Rprintf("%lu \t", seedArray[i]);
           seedArray[i] = ( (unsigned long long) seed << (i+1)/4 ) | ( (unsigned long long) seed >> (i/4+1) ) ^ ( (unsigned long long) seed << (i+1)/4 ) & ( (unsigned long long) seed >> (i/4+1) );
    }
    for(i = 0; i < length; i++)
        seedArray[i] = ( seed << (i+0)/4 ) & ( seed >> (i+1)/4 ) ^ ( (seed << (i+2)/4) ) | ( seed >> (i+3)/4 );

    
    for(i = 0; i < length-1; i++)
        seedArray[i] = i+1;
    seedArray[length-1] = seed;

    
    for(i=0;i<length;i++)
        Rprintf("- %lu %d %d %d %d \n", seedArray[i], i/4, (i+1)/4, (i+2)/4, (i+3)/4);
    
    Rprintf("\n");
*/    
    
/*    
    for(i = 0; i < length; i++)
        seedArray[i] =  seed * ( i+1) ;
    
    for(i=0;i<length;i++)
    Rprintf("- %lu \n", seedArray[i]);
    
    Rprintf("\n");
*/    
    // same initialisation as dSFMT 1.3.0 from Matsumoto and Saito
    seedArray[0] = seed;
    for (i = 1; i < length; i++) 
        seedArray[i] = 1812433253UL * ( seedArray[i - 1] ^ ( seedArray[i - 1] >> 30 ) ) + i;
    
    
   /*
    for(i=0;i<length;i++)
        Rprintf("- %lu \n", seedArray[i]);
    
    Rprintf("\n");
    */
    
    
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
	if (primeNumber[2] == 1) {
		for (i = 2; i < 100000; i++) {
			primeNumber[i] = primeNumber[i-1] + 2*primeNumber[i];
		}
	}
}

void get_primes(int *n, int *pri)
{
	int i;
	if (primeNumber[2] == 1) {
		reconstruct_primes();
	}
	for (i = 0; i < *n; i++) {
		pri[i] = primeNumber[i];
	}
}

