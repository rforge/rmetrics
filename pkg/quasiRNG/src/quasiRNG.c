/** 
 * @file  quasiRNG.c
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

#include "quasiRNG.h"

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
    if(prime == NULL)
    {
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
    //if(ismixed)        
    //    SFMT_init_gen_rand(seed);
    
    
    //u_ij is the Torus sequence term 
    //with n = state + i, s = j + 1, p = primeNumber[j] or prime[j]
    //u is stored column by column
    
    //if(ismixed) //SF Mersenne-Twister-mixed Torus algo
    //{ 
    //    for(j = 0; j < dim; j++)
    //    {    
    //        for(i = 0; i < nb; i++) 
    //        {
    //            state = gen_rand32();
    //            u[i + j * nb] = fracPart( state * sqrt( prime[j] ) ) ;
    //        }
    //    }
    //}
    //else //classic Torus algo
    //{
        if(usetime) //use the machine time
            state = seed;
        else 
            state  = offset;
        
        for(j = 0; j < dim; j++)
            for(i = 0; i < nb; i++) 
                u[i + j * nb] = fracPart( ( state + i ) * sqrt( prime[j] ) ) ;                
    //}
    
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

