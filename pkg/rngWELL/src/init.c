/**********************************************************************************************
 *   Copyright (c) 2009 Christophe Dutang and Petr Savicky                                    *
 *                                                                                            *
 *    This code can be used freely for personal, academic, or non-commercial purposes.        *
 *    For commercial purposes, please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca        *                                                          *
 *                                                                                            *                                                                                                 *
 **********************************************************************************************/
/*
 *  Torus algorithm to generate quasi random numbers 
 *
 *		init file
 *  
 *	Native routines registration, see 'writing R extensions'
 *
 */

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "rngWELL.h"

//table of registration
static const R_CallMethodDef callMethods[] = 
{
        {"doSetSeed4WELL", (DL_FUNC) &doSetSeed4WELL, 1},
        {"doWELL", (DL_FUNC) &doWELL, 5},
        {NULL, NULL, 0}
};

void R_init_rngWELL(DllInfo *info)
{
        //register method accessed with .Call
        R_registerRoutines(info, NULL, callMethods, NULL, NULL); 
        //make rngWELL C functions available from other packages
        R_RegisterCCallable("rngWELL", "setSeed4WELL", (DL_FUNC) setSeed4WELL);
        R_RegisterCCallable("rngWELL", "WELLrng", (DL_FUNC) WELLrng);
}
