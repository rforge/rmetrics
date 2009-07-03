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
 *		locale file
 *  
 *	'make' error messages
 *
 */
 

/* Localization */
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rngWELL", String)
#else
#define _(String) (String)
#endif

