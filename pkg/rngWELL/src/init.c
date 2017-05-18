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
/*
 *  Torus algorithm to generate quasi random numbers 
 *
 *		init file
 *  
 *	Native routines registration, see 'writing R extensions'
 *
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h> // for NULL

#include "rngWELL.h"
#include "version.h"


//table of registration routines accessed with .C()
static const R_CMethodDef cMethods[] = 
{
  {"initMT2002",      (DL_FUNC) &initMT2002, 3}, //rngWELL.h
  {"putRngWELL",      (DL_FUNC) &putRngWELL, 3}, //rngWELL.h
  {"getRngWELL",      (DL_FUNC) &getRngWELL, 3}, //rngWELL.h
  {"version_rngWELL", (DL_FUNC) &version_rngWELL, 1}, //version.h
  {NULL, NULL, 0}
};


//table of registration routines accessed with .Call()
static const R_CallMethodDef callMethods[] = 
{
  {"doSetSeed4WELL",  (DL_FUNC) &doSetSeed4WELL, 1}, //rngWELL.h
  {"doWELL",          (DL_FUNC) &doWELL, 5}, //rngWELL.h
  {NULL, NULL, 0}
};


//register method accessed with .C, .Call, .Fortran, .External respectively
void R_init_rngWELL(DllInfo *dll)
{
  //register method accessed with .Call
  R_registerRoutines(dll, cMethods, callMethods, NULL, NULL); 
  R_useDynamicSymbols(dll, FALSE); 
  
  //make rngWELL C functions available for other packages
  R_RegisterCCallable("rngWELL", "setSeed4WELL", (DL_FUNC) setSeed4WELL);
  R_RegisterCCallable("rngWELL", "WELLrng", (DL_FUNC) WELLrng);
  R_RegisterCCallable("rngWELL", "WELL_get_set_entry_point", (DL_FUNC) WELL_get_set_entry_point);
  /*R_RegisterCCallable("rngWELL", "initMT2002", (DL_FUNC) initMT2002);
   R_RegisterCCallable("rngWELL", "putRngWELL", (DL_FUNC) putRngWELL);
   R_RegisterCCallable("rngWELL", "getRngWELL", (DL_FUNC) getRngWELL);*/
}


