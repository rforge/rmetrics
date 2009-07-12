/** 
 * @file  congruRand.h
 * @brief header file for congruential RNG
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
 *  Congruential random number generators
 *    
 *      header file
 *
 */

//R header files
#include <R.h>
#include <Rmath.h>


double user_unif_rand_congru();
void user_unif_init_congru(unsigned int seed);

double get_congruRand();
int check_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed);
void set_congruRand(unsigned long long inp_mod, unsigned long long inp_mult,
		unsigned long long inp_incr, unsigned long long inp_seed);
void get_seed_congruRand(unsigned long long *out_seed);
void get_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);
void put_state_congru(double *pmod, double *pmult, double *pincr, double *pseed);

