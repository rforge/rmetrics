/**********************************************************************************************
 *   Copyright (c) 2009 Christophe Dutang and Petr Savicky                                    *
 *                                                                                            *
 *    This code can be used freely for personal, academic, or non-commercial purposes.        *
 *    For commercial purposes, please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca        *                                                          *
 *                                                                                            *                                                                                                 *
 **********************************************************************************************/
/*
 *  various Random Number Generators
 *
 *		C functions	
 *  
 *	Many ideas are taken from <Rsource>/src/main/RNG.c
 *
 */


#include "rngWELL.h"

/*********************************/
/*              constants               */
//the seed
static unsigned long seed; 
//a pseudo boolean to initiate the seed
static int isInit=0;
//the length (maximal) of the internal seed array for WELL44497
#define LENSEEDARRAY 1391
static unsigned int seedArray[LENSEEDARRAY];
//a pseudo boolean to initiate the seed array
static int isInitByArray=0;


/***********************************/
/* pseudo random generation */ 

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
    double *u = (double *) R_alloc(nb * dim, sizeof(double));
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

// call the WELL generator of L'Ecuyer
void WELLrng(double *u, int nb, int dim, int order, int temper, int version)
{
    int i, j;
    
    if(temper && order == 512)
        error(_("no tempering possible since it is useless, cf. Panneton et al.(2006)."));
    if(temper && order == 521)
        error(_("no tempering possible since it is useless, cf. Panneton et al.(2006)."));
    if(temper && order == 607)
        error(_("no tempering possible since it is useless, cf. Panneton et al.(2006)."));
    if(temper && order == 1024)
        error(_("no tempering possible since it is useless, cf. Panneton et al.(2006)."));
        
    if(version !=1 && version != 2)
        error(_("wrong version for WELL RNG, it must be 1 or 2."));
    
    switch (order) 
    {
    //no tempering for the first four RNGs
        case 512:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(16); 
            
            //init SFMT parameters
            InitWELLRNG512a( seedArray );        
    
            // compute u_ij
            for(j = 0; j < dim; j++)
                for(i = 0; i < nb; i++) 
                    u[i + j * nb] = WELLRNG512a(); // real on ]0,1[ interval
            break;
            
        case 521:            
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(17);
        
            if(version == 1)
            {
                //init SFMT parameters
                InitWELLRNG521a( seedArray );        

                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG521a(); // real on ]0,1[ interval
            }
            else
            {
                //init SFMT parameters
                InitWELLRNG521b( seedArray );        
                
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG521b(); // real on ]0,1[ interval
            }
            break;
            
       case 607:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(19);
            
            if(version == 1)
            {
                //init SFMT parameters
                InitWELLRNG607a( seedArray );        
                
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG607a(); // real on ]0,1[ interval
            }
            else
            {
                //init SFMT parameters
                InitWELLRNG607b( seedArray );        
                
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG607b(); // real on ]0,1[ interval                
            }
            break;
            
        case 1024:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(32); 
            
            if(version == 1)
            {
                //init SFMT parameters
                InitWELLRNG1024a( seedArray );        
                
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG1024a(); // real on ]0,1[ interval
            }
            else
            {
                //init SFMT parameters
                InitWELLRNG1024b( seedArray );        
                
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG1024b(); // real on ]0,1[ interval
            }            
            break;
        
    //tempering possible for these RNGs
        case 800:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(25); 
            
            if(temper == 0)
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG800a( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG800a(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG800b( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG800b(); // real on ]0,1[ interval
                }
            }
            else
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG800aTemp( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG800aTemp(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG800bTemp( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG800bTemp(); // real on ]0,1[ interval
                }
            }
            break;
            
        case 19937:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(624); 
            
            if(temper == 0)
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG19937a( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG19937a(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG19937b( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG19937b(); // real on ]0,1[ interval
                }
            }
            else
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG19937aTemp( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG19937aTemp(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG19937bTemp( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG19937bTemp(); // real on ]0,1[ interval
                }
            }
            break;
        
        case 21701:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(679); 
            
            if(temper == 0)
            {                
                //init SFMT parameters
                InitWELLRNG21701a( seedArray );       
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG21701a(); // real on ]0,1[ interval
            }
            else
            {
               
                //init SFMT parameters
                InitWELLRNG21701aTemp( seedArray );       
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG21701aTemp(); // real on ]0,1[ interval
            }
            break;
            
        case 23209:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(726); 
            
            if(temper == 0)
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG23209a( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG23209a(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG23209b( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG23209b(); // real on ]0,1[ interval
                }
            }
            else
            {
                if(version == 1)
                {
                    //init SFMT parameters
                    InitWELLRNG23209aTemp( seedArray );       
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG23209aTemp(); // real on ]0,1[ interval
                }
                else
                {
                    //init SFMT parameters
                    InitWELLRNG23209bTemp( seedArray );        
                    // compute u_ij
                    for(j = 0; j < dim; j++)
                        for(i = 0; i < nb; i++) 
                            u[i + j * nb] = WELLRNG23209bTemp(); // real on ]0,1[ interval
                }
            }
            break;    
            
        case 44497:
            //initiate the seed with the machine time
            // and ensure it is positive
            if(!isInitByArray) 
                randSeedByArray(1391); 
            
            if(temper == 0)
            {                
                //init SFMT parameters
                InitWELLRNG44497a( seedArray );       
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG44497a(); // real on ]0,1[ interval
            }
            else
            {
                
                //init SFMT parameters
                InitWELLRNG44497aTemp( seedArray );       
                // compute u_ij
                for(j = 0; j < dim; j++)
                    for(i = 0; i < nb; i++) 
                        u[i + j * nb] = WELLRNG44497aTemp(); // real on ]0,1[ interval
            }
            break;
            
        default:
            error(_("error wrong exponent in WELL generator\n"));
    }
    
    isInitByArray = 0;	          
}


/**********************************/
/*          set the seed                */

//main function used .Call()
//seed set by the user
//idea taken from the R internal C function do_setseed4WELL
SEXP doSetSeed4WELL(SEXP s)
{
    if (!isNumeric(s))
        error(_("invalid argument"));
	
    setSeed4WELL( (long) asInteger(s) );
    
    return R_NilValue;	
}

void setSeed4WELL(long s)
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

