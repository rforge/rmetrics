/*
C ##############################################################################
C PART I: HALTON SEQUENCE:
C ##############################################################################

C--------------------------------------------------------------------------
C @file  LowDiscrepancy.f
C @brief Halton sequence
C
C @author Diethelm Wuertz
C @author Christophe Dutang
C
C Copyright (C) Aug. 2016, Christophe Dutang, C translation of the Fortran code
C
C Copyright (C) Apr. 2011, Christophe Dutang, remove implicit declaration: the code now pass
C > gfortran -c -fsyntax-only -fimplicit-none LowDiscrepancy.f
C without error.
C
C Copyright (C) Oct. 2009, Christophe Dutang, slightly modified (better accuracy and speed).
C
C Copyright (C) Sept. 2002, Diethelm Wuertz, ETH Zurich. All rights reserved.
C
C The new BSD License is applied to this software.
C Copyright (c) Diethelm Wuertz, ETH Zurich. All rights reserved.
C
C      Redistribution and use in source and binary forms, with or without
C      modification, are permitted provided that the followingConditions are
C      met:
C
C          - Redistributions of sourceCode must retain the aboveCopyright
C          notice, this list ofConditions and the following disclaimer.
C          - Redistributions in binary form must reproduce the above
C         Copyright notice, this list ofConditions and the following
C          disclaimer in the documentation and/or other materials provided
C          with the distribution.
C          - Neither the name of the ETH Zurich nor the names of its Contributors
C          may be used to endorse or promote products derived from this software
C          without specific prior written permission.
C
C      THIS SOFTWARE IS PROVIDED BY THECOPYRIGHT HOLDERS ANDCONTRIBUTORS
C      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
C      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
C      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THECOPYRIGHT
C      OWNER ORCONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
C      SPECIAL, EXEMPLARY, ORCONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
C      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
C      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVERCAUSED AND ON ANY
C      THEORY OF LIABILITY, WHETHER INCONTRACT, STRICT LIABILITY, OR TORT
C      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
C      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
C  
C-------------------------------------------------------------------------- 
*/

#include "LowDiscrepancy-halton.h"

/*********************************/
/*       constants               */


//the first 100 000 prime numbers computed from their differences stored in primes.h
static int primeNumber[100000];


//old translation of Fortran code

void INITHALTON(int DIMEN, double *QUASI)
{
    //in Fortran code BASE contains prime numbers
    int ITER, DIGIT;
    double HALF;
    int OFFSET = 0;
    
    int NB;
    
    //create the first quasi random number for all dimensions
    for(NB = 0; NB < DIMEN; NB++)
    {
        ITER = OFFSET;
        QUASI[NB] = 0.0;
        HALF = 1.0 / primeNumber[NB];
        do
        {
            DIGIT = ITER % primeNumber[NB];
            QUASI[NB] = QUASI[NB] + DIGIT * HALF;
            ITER = ( ITER - DIGIT ) / primeNumber[NB];
            HALF = HALF / primeNumber[NB];
        } while (ITER != 0);
    }
    OFFSET = OFFSET + 1;
    
}

//compute the radical inverse function of integer OFFSET with base primeNumber[DIMEN]
double HALTONREC(int DIMEN, unsigned long long OFFSET)
{
    //in Fortran code BASE contains prime numbers
    unsigned long long ITER, DIGIT;
    double HALF;
    double QUASI;
    
    ITER = OFFSET;
    QUASI = 0.0;
    HALF = 1.0 / primeNumber[DIMEN];
    if (primeNumber[2] == 1)
      reconstruct_prime();
    //Rprintf("DIMEN %u, prime %u\n", DIMEN, primeNumber[DIMEN]);
    do
    {
        DIGIT = ITER % primeNumber[DIMEN];
        QUASI = QUASI + DIGIT * HALF;
        ITER = ( ITER - DIGIT ) / primeNumber[DIMEN];
        HALF = HALF / primeNumber[DIMEN];
    } while (ITER != 0);
    return QUASI;
}

#include "primes.h"
void reconstruct_prime()
{
  int i;
  if (primeNumber[2] == 1)
    for (i = 2; i < 100000; i++)
      primeNumber[i] = primeNumber[i-1] + 2*primeNumber[i];
}


