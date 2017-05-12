/** 
 * @file  init.c
 * @brief init file for all RNGs
 *
 * @author Christophe Dutang
 * @author Petr Savicky 
 *
 *
 * Copyright (C) 2017, Christophe Dutang, 
 * Petr Savicky, Academy of Sciences of the Czech Republic. 
 * All rights reserved.
 *
 * The new BSD License is applied to this software.
 * Copyright (c) 2013 Christophe Dutang, Petr Savicky. 
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
 *  randtoolbox to generate pseudo and quasi random sequences
 *
 *		init file
 *  
 *	Native routines registration, see 'writing R extensions'
 *
 */

#include <stdlib.h> //for NULL
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "randtoolbox.h"
#include "runifInterface.h"
#include "mt19937ar.h"
#include "testrng.h"
#include "version.h"

//table of registration routines accessed with .C()
static const R_CMethodDef CEntries[] = {
  {"set_noop",                    (DL_FUNC) &set_noop, 0}, //runifInterface.h
  {"current_generator",           (DL_FUNC) &current_generator, 1}, //runifInterface.h
  {"put_user_unif_set_generator", (DL_FUNC) &put_user_unif_set_generator, 0}, //runifInterface.h
  {"get_state_congru",            (DL_FUNC) &get_state_congru, 2}, //congruRand.h
  {"put_state_congru",            (DL_FUNC) &put_state_congru, 3}, //congruRand.h
  {"initMersenneTwister",         (DL_FUNC) &initMersenneTwister, 4}, //mt19937ar.h
  {"putMersenneTwister",          (DL_FUNC) &putMersenneTwister, 3}, //mt19937ar.h 
  {"getMersenneTwister",          (DL_FUNC) &getMersenneTwister, 3}, //mt19937ar.h 
  {"get_primes",                  (DL_FUNC) &get_primes, 2}, //randtoolbox.h
  {"version_randtoolbox",         (DL_FUNC) &version_randtoolbox, 1}, //version.h
  {NULL, NULL, 0}
};


//table of registration routines accessed with .Call()
static const R_CallMethodDef CallEntries[] = 
{
  {"doCongruRand",        (DL_FUNC) &doCongruRand, 6}, //randtoolbox.h
  {"doHalton",            (DL_FUNC) &doHalton, 6}, //randtoolbox.h
  {"doKnuthTAOCP",        (DL_FUNC) &doKnuthTAOCP, 2}, //randtoolbox.h
  {"doSetSeed",           (DL_FUNC) &doSetSeed, 1}, //randtoolbox.h
  {"doSFMersenneTwister", (DL_FUNC) &doSFMersenneTwister, 4}, //randtoolbox.h
  {"doSobol",             (DL_FUNC) &doSobol, 6}, //randtoolbox.h
  {"doTorus",             (DL_FUNC) &doTorus, 7}, //randtoolbox.h
  {"doWELL",              (DL_FUNC) &doWELL, 5}, //randtoolbox.h
  {"doPokerTest",         (DL_FUNC) &doPokerTest, 3}, //testrng.h
  {"doCollisionTest",     (DL_FUNC) &doCollisionTest, 3}, //testrng.h
  {NULL, NULL, 0}
};


/* .Fortran calls defined LowDiscrepancy.f 
 * C version of these Fortran routines are halton_c() and sobol_c() in randtoolbox.c
 */
/* DOES NOT WORK
extern void F77_NAME(halton_f)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(sobol_f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

//table of registration routines accessed with .Fortran()
static const R_FortranMethodDef FortranEntries[] = {
  {"halton_f", (DL_FUNC) &F77_NAME(halton_f),  7},
  {"sobol_f", (DL_FUNC) &F77_NAME(sobol_f),  11},
  {NULL, NULL, 0}
};*/


//there is no routine accessed with .External()

//table of all registered routines
void R_init_randtoolbox(DllInfo *dll)
{
  //register method accessed with .C, .Call, .Fortran, .External respectively
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL); 
  R_useDynamicSymbols(dll, TRUE); 
  
  //make randtoolbox C functions available for other packages
  R_RegisterCCallable("randtoolbox", "torus", (DL_FUNC) torus);
  R_RegisterCCallable("randtoolbox", "halton_c", (DL_FUNC) halton_c);
  R_RegisterCCallable("randtoolbox", "sobol_c", (DL_FUNC) halton_c);
  R_RegisterCCallable("randtoolbox", "setSeed", (DL_FUNC) setSeed);
  R_RegisterCCallable("randtoolbox", "congruRand", (DL_FUNC) congruRand);
  R_RegisterCCallable("randtoolbox", "SFmersennetwister", (DL_FUNC) SFmersennetwister);
  R_RegisterCCallable("randtoolbox", "pokerTest", (DL_FUNC) pokerTest);
  R_RegisterCCallable("randtoolbox", "collisionTest", (DL_FUNC) collisionTest);
  R_RegisterCCallable("randtoolbox", "knuthTAOCP", (DL_FUNC) knuthTAOCP); 
  
  //retrieve WELL rng entry point in the rngWELL pkg
  WELLrng = (void (*) (double *, int, int, int, int, int)) R_GetCCallable("rngWELL", "WELLrng");
  WELL_get_set_entry_point =(void (*) (void (*)())) R_GetCCallable("rngWELL", "WELL_get_set_entry_point");
  /*getRngWELL = (void (*) (int *, int *, unsigned int *)) R_GetCCallable("rngWELL", "getRngWELL");
   putRngWELL = (void (*) (int *, int *, unsigned int *)) R_GetCCallable("rngWELL", "putRngWELL");
   initMT2002 = (void (*) (unsigned int *, int *, unsigned int *)) R_GetCCallable("rngWELL", "initMT2002");*/
  
}

