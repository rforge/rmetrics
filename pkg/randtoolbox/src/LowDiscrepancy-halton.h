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


//R header files
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


/* utility functions to be called in randtoolbox.c */

double HALTONREC(int DIMEN, int OFFSET);
void reconstruct_prime(); //same as void reconstruct_primes()
