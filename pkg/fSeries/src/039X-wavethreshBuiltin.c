#include <stdio.h>

/* #include "wavelet.h" */

#include <R.h>

#define PERIODIC    1
#define SYMMETRIC   2

int reflect(int n, int lengthC, int bc);


void convolveC(
    double *c_in,   /* Input data                       */
    int LengthCin,  /* Length of this array             */
    int firstCin,   /* 1st index of input               */

    double *H,      /* Filter                           */
    int LengthH,    /* Length of filter                 */

    double *c_out,  /* Output data                      */
    int LengthCout, /* Length of above array            */
    int firstCout,  /* First index of C array           */
    int lastCout,   /* Last index of C array            */
    int bc          /* Method of boundary correction:
                       PERIODIC, SYMMETRIC              */
    );

void convolveD(
    double *c_in,   /* Input data                       */
    int LengthCin,  /* Length of this array             */
    int firstCin,   /* 1st index of input               */

    double *H,      /* Filter                           */
    int LengthH,    /* Length of filter                 */

    double *d_out,  /* Output data                      */
    int LengthDout, /* Length of above array            */
    int firstDout,  /* First index of D array           */
    int lastDout,   /* Last index of D array            */
    int bc          /* Method of boundary correction:
                       PERIODIC or SYMMETRIC            */
    );

void wavedecomp(
    double *C,      /* Input data, and the subsequent smoothed data */
    Sint *LengthC,  /* Length of C array                            */
    double *D,      /* The wavelet coefficients                     */
    Sint *LengthD,  /* Length of D array                            */
    double *H,      /* The smoothing filter H                       */
    Sint *LengthH,  /* Length of smoothing filter                   */
    Sint *levels,   /* The number of levels in this decomposition   */
    Sint *firstC,   /* The first possible C coef at a given level   */
    Sint *lastC,    /* The last possible C coef at a given level    */
    Sint *offsetC,  /* Offset from C[0] for certain level's coeffs  */
    Sint *firstD,   /* The first possible D coef at a given level   */
    Sint *lastD,    /* The last possible D coef at a given level    */
    Sint *offsetD,  /* Offset from D[0] for certain level's coeffs  */
    Sint *bc,       /* Method of boundary correction                */
    Sint *ierr      /* Error code                                   */
    );


/* ************************************************************************** */


/*
 * CONVOLVE -   Do filter H filter convolution with boundary
 */

#include <stdio.h>

/* #include "wavelet.h" */


/*
 * ACCESSC handles negative accesses, as well as those that exceed the number
 * of elements
 */

#define ACCESSC(c, firstC, lengthC, ix, bc) \
    *(c+reflect(((ix)-(firstC)),(lengthC),(bc)))


void convolveC(
    double *c_in,   /* Input data                   */
    int LengthCin,  /* Length of this array         */
    int firstCin,   /* 1st index of input           */

    double *H,      /* Filter                       */
    int LengthH,    /* Length of filter             */

    double *c_out,  /* Output data                  */
    int LengthCout, /* Length of above array        */
    int firstCout,  /* First index of C array       */
    int lastCout,   /* Last index of C array        */
    int bc          /* Method of boundary correction:
                       PERIODIC, SYMMETRIC          */
    )
{
    double sum;
    register int k;
    register int count_out;
    register int m;

    count_out = 0;
    for(k=firstCout; k <= lastCout; ++k)    {

    sum = 0.0;
    for(m=0; m < LengthH; ++m)
        sum += *(H+m) * ACCESSC(c_in, firstCin, LengthCin, (m+2*k),bc);

    *(c_out + count_out) = sum;
    ++count_out;
    }
}

void convolveD(
    double *c_in,   /* Input data                   */
    int LengthCin,  /* Length of this array         */
    int firstCin,   /* 1st index of input           */

    double *H,      /* Filter                       */
    int LengthH,    /* Length of filter             */

    double *d_out,  /* Output data                  */
    int LengthDout, /* Length of above array        */
    int firstDout,  /* First index of D array       */
    int lastDout,   /* Last index of D array        */
    int bc          /* Method of boundary correction:
                    PERIODIC or SYMMETRIC           */
    )
{
    double sum;
    register int k;
    register int count_out;
    register int m;

    count_out = 0;

    for(k=firstDout; k<=lastDout; ++k)  {

    sum = 0.0;
    for(m=0; m < LengthH; ++m)  {

        if (m & 1)  /* odd */
        sum += *(H+m) * ACCESSC(c_in, firstCin, LengthCin,(2*k+1-m),bc);
        else
        sum -= *(H+m) * ACCESSC(c_in, firstCin, LengthCin,(2*k+1-m),bc);
    }

    *(d_out + count_out) = sum;
    ++count_out;
    }
}


/* Works out reflection, as REFLECT, but reports access errors */
int reflect(
    int n,
    int lengthC,
    int bc)
{

/* do not really exit()! -- would take down S/R as well!
 * the return() is just for -Wall .. */
#define Exit(i) { error("convolveC: error exit (%d)", i); return(-1); }

    if ((n >= 0) && (n < lengthC))
    return(n);
    else if (n<0)   {
    if (bc==PERIODIC)   {
        /*
          n = lengthC+n;
        */
        n = n%lengthC + lengthC*((n%lengthC)!=0);
        if (n < 0)      {
        REprintf("reflect: access error (%d,%d)\n", n,lengthC);
        REprintf("reflect: left info from right\n");
        Exit(2);
        }
        else
        return(n);
    }

    else if (bc==SYMMETRIC) {
        n = -1-n;
        if (n >= lengthC)       {
        REprintf("reflect: access error (%d,%d)\n",
             n,lengthC);
        Exit(3);
        }
        else
        return(n);
    }

    else    {
        REprintf("reflect: Unknown boundary correction");
        REprintf(" value of %d\n", bc);
        Exit(4);
    }

    }
    else    {
    if (bc==PERIODIC)   {
        /*
          printf("periodic extension, was %d (%d) now ",n,lengthC);
          n = n - lengthC;
        */
        n %= lengthC;
        /*
          printf("%d\n", n);
        */
        if (n >= lengthC)   {
        REprintf("reflect: access error (%d,%d)\n",
            n,lengthC);
        REprintf("reflect: right info from left\n");
        Exit(5);
        }
        else
        return(n);
    }
    else if (bc==SYMMETRIC) {
        n = 2*lengthC - n - 1;
        if (n<0)        {
        REprintf("reflect: access error (%d,%d)\n",
            n,lengthC);
        Exit(6);
        }
        else
        return(n);
    }
    else    {
        REprintf("reflect: Unknown boundary correction\n");
        Exit(7);
    }
    }

/* Safety */
    REprintf("reflect: SHOULD NOT HAVE REACHED THIS POINT\n");
    Exit(8);
}


/* ************************************************************************** */


#define ACCESSC(l,r)    *(C + *(offsetC+(l)) + (r) - *(firstC+(l)))
#define ACCESSD(l,r)    *(D + *(offsetD+(l)) + (r) - *(firstD+(l)))


void wavedecomp(
    double *C,          /* Input data, and the subsequent smoothed data */
    Sint *LengthC,      /* Length of C array                            */
    double *D,          /* The wavelet coefficients                     */
    Sint *LengthD,      /* Length of D array                            */
    double *H,          /* The smoothing filter H                       */
    Sint *LengthH,      /* Length of smoothing filter                   */
    Sint *levels,       /* The number of levels in this decomposition   */
    Sint *firstC,       /* The first possible C coef at a given level   */
    Sint *lastC,        /* The last possible C coef at a given level    */
    Sint *offsetC,      /* Offset from C[0] for certain level's coeffs  */
    Sint *firstD,       /* The first possible D coef at a given level   */
    Sint *lastD,        /* The last possible D coef at a given level    */
    Sint *offsetD,      /* Offset from D[0] for certain level's coeffs  */
    Sint *bc,           /* Method of boundary correction                */
    Sint *ierr          /* Error code                                   */
    )
{
    register int next_level, at_level;
    register int verbose; /* Controls message printing, passed in ierr var*/

    if (*ierr == 1)
    verbose = 1;
    else
    verbose = 0;

    if (verbose)    {
    if (*bc == PERIODIC)
        printf("Periodic boundary method\n");
    else if (*bc == SYMMETRIC)
        printf("Symmetric boundary method\n");
    else    {
        printf("Unknown boundary correction method\n");
        *ierr = 1;
        return;
    }
    printf("Decomposing into level: ");
    }

    *ierr = 0;

    for(next_level = *levels - 1; next_level >= 0; --next_level)    {

    if (verbose)
        printf("%d ", next_level);

    at_level = next_level + 1;

    convolveC( (C+*(offsetC+at_level)),
           (int)(*(lastC+ at_level) - *(firstC+at_level)+1),
           (int)(*(firstC+at_level)),
           H,
           (int)*LengthH,
           (C+*(offsetC+next_level)),
           (int)(*(lastC+next_level) - *(firstC+next_level)+1),
           (int)(*(firstC+next_level)),
           (int)(*(lastC+next_level)) , (int)*bc);

    convolveD( (C+*(offsetC+at_level)),
           (int)(*(lastC+ at_level) - *(firstC+at_level)+1),
           (int)(*(firstC+at_level)),
           H,
           (int)*LengthH,
           (D+*(offsetD+next_level)),
           (int)(*(lastD+next_level) - *(lastD+next_level)+1),
           (int)(*(firstD+next_level)),
           (int)(*(lastD+next_level)), (int)*bc );
    }
    if (verbose)
    printf("\n");
    return;
}


/* ************************************************************************** */

