/** 
 * @file  init.c
 * @brief init file for all RNGs
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
 *  randtoolbox to generate pseudo and quasi random sequences
 *
 *		init file
 *  
 *	Native routines registration, see 'writing R extensions'
 *
 */

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "randtoolbox.h"
#include "testrng.h"

//table of registration
static const R_CallMethodDef callMethods[] = 
{
        {"doTorus", (DL_FUNC) &doTorus, 6},
        {"doSetSeed", (DL_FUNC) &doSetSeed, 1},
        {"doCongruRand", (DL_FUNC) &doCongruRand, 6},
        {"doSFMersenneTwister", (DL_FUNC) &doSFMersenneTwister, 4},
        {"doPokerTest", (DL_FUNC) &doPokerTest, 3},
        {"doCollisionTest", (DL_FUNC) &doCollisionTest, 3},
        {"doWELL", (DL_FUNC) &doWELL, 5},
        {"doKnuthTAOCP", (DL_FUNC) &doKnuthTAOCP, 2},
        {NULL, NULL, 0}
};


//table of registered routines
void R_init_randtoolbox(DllInfo *info)
{
        //register method accessed with .Call
        R_registerRoutines(info, NULL, callMethods, NULL, NULL); 
		
        //make randtoolbox C functions available from other packages
        R_RegisterCCallable("randtoolbox", "torus", (DL_FUNC) torus);
        R_RegisterCCallable("randtoolbox", "setSeed", (DL_FUNC) setSeed);
        R_RegisterCCallable("randtoolbox", "congruRand", (DL_FUNC) congruRand);
        R_RegisterCCallable("randtoolbox", "SFmersennetwister", (DL_FUNC) SFmersennetwister);
        R_RegisterCCallable("randtoolbox", "pokerTest", (DL_FUNC) pokerTest);
        R_RegisterCCallable("randtoolbox", "collisionTest", (DL_FUNC) collisionTest);
        R_RegisterCCallable("randtoolbox", "knuthTAOCP", (DL_FUNC) knuthTAOCP); 
		 
		//retrieve WELL rng entry point in the rngWELL pkg
		WELLrng = (void (*) (double *, int, int, int, int, int)) R_GetCCallable("rngWELL", "WELLrng");
  
}



