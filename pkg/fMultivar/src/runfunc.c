/*===========================================================================*/
/* Written by Jarek Tuszynski. Copyright 2001-2005 SAIC.                     */
/* Software developed in conjunction with the National Cancer Institute, and */
/* distributed under "caBIO Software License" included in "COPYING" file.    */
/*===========================================================================*/

/*
#The caMassClass Software License, Version 1.0
#(see "http://ncicb.nci.nih.gov/download/camassclasslicense.jsp" for original)
#Copyright 2001-2003 SAIC. This software was developed in conjunction with the 
#National Cancer Institute, and so to the extent government employees are 
#co-authors,any rights in such works shall be subject to Title 17 of the United 
#States Code, section 105. 
#Redistribution and use in source and binary forms,with or without modification, 
#are permitted provided that the following conditions are met: 
#1. Redistributions of source code must retain the above copyright notice, this 
#list of conditions and the disclaimer of Article 3, below. Redistributions in 
#binary form must reproduce the above copyright notice, this list of conditions 
#and the following disclaimer in the documentation and/or other materials 
#provided with the distribution. 
#2. The end-user documentation included with the redistribution, if any, must 
#include the following acknowledgment:
#"This product includes software developed by the SAIC and the National Cancer 
#Institute."
#If no such end-user documentation is to be included, this acknowledgment shall 
#appear in the software itself, wherever such third-party acknowledgments 
#normally appear. 
#3. The names "The National Cancer Institute","NCI" and "SAIC" must not be used 
#to endorse or promote products derived from this software. 
#4. This license does not authorize the incorporation of this software into any 
#third party proprietary programs. This license does not authorize the recipient 
#to use any trademarks owned by either NCI or SAIC-Frederick. 
#5. THIS SOFTWARE IS PROVIDED "AS IS," AND ANY EXPRESSED OR IMPLIED WARRANTIES, 
#(INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
#FITNESS FOR A PARTICULAR PURPOSE) ARE DISCLAIMED.IN NO EVENT SHALL THE NATIONAL
#CANCER INSTITUTE, SAIC, OR THEIR AFFILIATES BE LIABLE FOR ANY DIRECT, INDIRECT, 
#INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
#LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
#PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
#LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE 
#OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
#ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>

#ifdef DEBBUG
  int R_finite(double x) {return 1;}
  #define Calloc(b, t)  (t*) calloc(b,sizeof(t))
  #define Free free
#else
  #include <R.h>
  #include <Rinternals.h>
#endif

/*========================================================================================*/
/* Each iteration of an insertion sort removes an element from the input data, inserting  */
/*  it at the correct position in the already sorted list, until no elements are left     */
/* in the input. For unsorted data is much less efficient than the more advanced          */
/* algorithms such as Quicksort, Heapsort, or Merge sort, but it is very efficient on     */
/* data sets which are already substantially sorted (almost O(n))                         */
/* Referances:                                                                            */
/*   R. Sedgewick: Algorithms. Addison-Wesley (1988) (page 99)                            */
/*   http://en.wikipedia.org/wiki/Insertion_sort                                          */
/*   http://www.cs.ubc.ca/spider/harrison/Java/sorting-demo.html                          */
/* Input:                                                                                 */
/*   V    - data array we operate on remains unchanged (we assume that no number in idx   */
/*          array is longer than V                                                        */
/*   idx  - index numbers of elements in V to be partially sorted                         */
/*   nIdx - length of idx array                                                           */
/* Output:                                                                                */
/*   idx  - index numbers of sorted elements in V w                                       */
/*========================================================================================*/

void insertion_sort(const double *V, int *idx, const int nIdx) 
{ 
  int i, j, id;
  double v;
  for (i=1; i<nIdx; i++) {
    id = idx[i];
    v  = V[id];
    for(j=i; j>0; j--) {
      if (V[idx[j-1]]<v) break;
      idx[j] = idx[j-1];
    }
    idx[j] = id;
  }
}


/*============================================================================*/
/* The following function was inspired by msum from                           */
/* http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/393090             */
/* Quote from it:                                                             */
/* "Full precision summation using multiple doubles for intermediate values   */
/* Rounded x+y stored in hi with the round-off stored in lo.  Together        */
/* hi+lo are exactly equal to x+y.  The loop applies hi/lo summation          */
/* to each partial so that the list of partial sums remains exact.            */
/* Depends on IEEE-754 arithmetic guarantees.  See proof of correctness at:   */
/* www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps"  */
/*============================================================================*/
#define SumErr(a,b,x) (((a>b) == (a>-b)) ?  b - (x-a) : a - (x-b) )
#define mpartial 1024	
#define add2partials(x, partial, npartial) {                 \
  int j, i;                                                  \
  double hi, lo, y;                                          \
  for (i=j=0; j<npartial; j++) {                             \
    y  = partial[j];                                         \
    hi = y + x;                                              \
    lo = (((x>y) == (x>-y)) ?  y - (hi-x) : x - (hi-y) );    \
    if (lo && i<mpartial) partial[i++] = lo;                 \
    x = hi;                                                  \
  }                                                          \
  partial[i] = x;                                            \
  npartial   = i+1;                                          \
}

void sum_exact(double *In, double *Out, const int *nIn)
{
  int i, j, n=*nIn, npartial=0;
  double *in, x, partial[mpartial];
  in=In;
  for(i=0; i<n; i++, in++) {
    x = *in;
    add2partials(x, partial, npartial);
  }
  *Out = partial[0];
  for(j=1; j<npartial; j++) *Out += partial[j];
}

void cumsum_exact(double *In, double *Out, const int *nIn)
{
  int i, j, n=*nIn, npartial=0;
  double *in, *out, x, partial[mpartial];
  in=In; out=Out;
  for(i=0; i<n; i++, in++, out++) {
    x = *in;
    add2partials(x, partial, npartial);
    *out = partial[0];
    for(j=1; j<npartial; j++) *out += partial[j];
  }
}

void runsum(double *In, double *Out, int *Size, const int *nIn, const int *nWin)
{
  int i, j, k, n=*nIn, m=*nWin, npartial=0, count=0, *size;
  double *in, *out, partial[mpartial], x;
  k = m/2;                              /* half of moving window */                           
  for(i=0; i<=k; i++) {
    Out [i]=Out [n-i-1]=0;
    Size[i]=Size[n-i-1]=0;
  }
  if (m>=n) return;

  /* step 1: sum of the first window *out = sum(x[0:(m-1)]) + err1 */
  in=In; out=Out+k; size=Size+k;
  for(i=0; i<m; i++, in++) {
    x = *in;
    if (R_finite(x)) {
      add2partials(x, partial, npartial);
      count++;
    }
  }
  *size = count;
  *out  = partial[0];
  for(j=1; j<npartial; j++) *out += partial[j];

  /* step 2: runsum of the rest of the vector. Inside loop is same as:   */
  /* *out = *(out-1) + *in - *(in-m); but with round of error correction */
  out++; size++;
  for(i=m; i<n; i++, out++, in++, size++) { 
    x = *in;    /* add the new value */
    if (R_finite(x)) {
      add2partials(x, partial, npartial);
      count++;
    }
    x = -*(in-m); /* drop the value that goes out of the window */
    if (R_finite(x)) {
	    add2partials(x, partial, npartial);
	    count--;
    }
    *size = count;
    *out  = partial[0];
    for(j=1; j<npartial; j++) *out += partial[j];
  }
}

/*==================================================================================*/
/* Mean function applied to (running) window. All additions performed using         */
/* addition algorithm which tracks and corrects addition round-off errors (see      */  
/*  http://www-2.cs.cmu.edu/afs/cs/project/quake/public/papers/robust-arithmetic.ps)*/
/* Input :                                                                          */
/*   In   - array to run moving window over will remain umchanged                   */
/*   Out  - empty space for array to store the results                              */
/*   nIn  - size of arrays In and Out                                               */
/*   nWin - size of the moving window                                               */
/* Output :                                                                         */
/*   Out  - results of runing moving window over array In and colecting window mean */
/*==================================================================================*/
void runmean(double *In, double *Out, const int *nIn, const int *nWin)
{
  int i, k, n=*nIn, m=*nWin;
  double *in, *out, d, x, err, err1=0;
  k = m/2;                              /* half of moving window */                           
  for(i=0; i<=k; i++) Out[i]=Out[n-i-1]=0;
  if (m>=n) return; 
  /* step 1: sum of the first window *out = sum(x[0:(m-1)]) + err1 */
  in=In; out=Out+k; 
  for(i=0; i<m; i++, in++) {
    d = *out;
    *out += *in; /* sum of initial window out[k]=sum(in[0:m-1]) */
    err1 += SumErr(d, *in, *out);
  }
  /* step 2: runsum of the rest of the vector. Inside loop is same as:   */
  /* *out = *(out-1) + *in - *(in-m); but with round of error correction */
  for(out++; i<n; i++, out++, in++) {      /* "in" already set: in=In+m  */
    x    = - *(in-m);
    d    = *(out-1) + *in;
    *out = d + x; 
    err  = SumErr(*(out-1),  *in , d) + SumErr( d,  x, *out);
    *(out-1) += err1;
    err1 += err;
  }
  *(out-1) += err1;
  /* step 3: runmean = runsum/m */
  d = 1.0/m;
  out=Out+k;   
  for(i=m; i<=n; i++) *(out++) *= d;  /* out = out/m */
}
#undef SumErr
#undef mpartial
#undef add2partials

/*==================================================================*/
/* quantile function applied to (running) window                    */ 
/* Input :                                                          */
/*   In   - array to run moving window over will remain umchanged   */
/*   Out  - empty space for array to store the results. Out is      */
/*          assumed to have reserved memory for nIn*nProbs elements */
/*   nIn  - size of arrays In and Out                               */
/*   nWin - size of the moving window (odd)                         */
/*   Prob - in the moving window with sorted elements X[1]:X[nWin]  */
/*          which element to return? 1 is minimum, nWin is maximum, */
/*          (nWin+1)/2 is median, non-integers cause interpolation  */
/*          multiple probs are allowed.                             */
/*   nProb - How many elements in Probs array?                      */
/* Output :                                                         */
/*   Out  - results of runing moving window over array In and       */
/*          colecting window mean                                   */
/*==================================================================*/

void runquantile(double *In, double *Out, const int *nIn, const int *nWin, const double *Prob, const int *nProb)
{
  int i, j, k, *idx, d, n=*nIn, m=*nWin, nPrb=*nProb;
  double *Win, *in, *out, pointOut, ext, r, ip;
  k   = m/2;                          /* half of window size */
  in  = In;
  out = Out+k;

  if (nPrb==1 && (*Prob==1 || *Prob==m)) { /* trivial case shortcut - if prob is 0 or 1 than wind windows min or max */
    d = (*Prob==1 ? -1 : 1);          /* runmin d=-1; runmax d=1*/
    pointOut=ext=0;
    for(i=m-1; i<n; i++) {
      if(pointOut==ext) {             /* if point comining out of the window was window's extreme than ... */
        ext=in[0];                    /* in the 2 lines below we do things diffferent when extreme is a max vs. min */
        if (d==1) { 
          for(j=1; j<m; j++) if (ext<in[j]) ext=in[j];  /* find maximum over a window of length m */
        } else {   
          for(j=1; j<m; j++) if (ext>in[j]) ext=in[j];  /* find minimum over a window of length m */
        } 
      } else                          /* if point comining out of the window was NOT window extreme than we know ... */
        if (ext*d<in[m-1]*d) ext=in[m-1]; /* ... extreme of window's first m-1 points, so we have to add a single point */
      pointOut = *(in++);             /* store point comming out of the window for future use and move window */
      *(out++) = ext;                 /* and fill the space with window extreme and move window */
    }
  } else {                            /* non-trivial case */
    idx = Calloc(m,int   );           /* index will hold partially sorted index numbers of Save array */
    Win = Calloc(m,double);           /* stores all points of the current running window */
    for(i=0; i<m; i++) {
      Win[i] = *(in++);               /* initialize running window */
      idx[i] = i;                     /* and its index */
    }
    in--;                             /* last point of the window will be placed again */
    for(j=i=m-1; i<n; i++) {
      Win[j] = *(in++);               /* Move Win to the right: replace a[i-m] with a[m] point  */
      insertion_sort(Win,idx,m);      /* sort current Win */
      for(d=0; d<nPrb; d++) {
        r = modf( Prob[d], &ip );     /* Divide p into its fractional and integer parts            */
        k = (int) ip-1;               /* k-1 instead of k because in C arrays are 0 based and in R they are 1 based */
        if (r) r = Win[idx[k]]*(1-r) + Win[idx[k+1]]*r; /* interpolate */
        else   r = Win[idx[k]];  
        *(out+d*n) = r;
      }
      out++;
      j = (j+1)%m;                    /* index goes from 0 to m-1, and back to 0 again  */
    }
    Free(Win);
    Free(idx);
  }
}

/*==================================================================================*/
/* MAD function applied to moving (running) window                                  */ 
/* Input :                                                                          */
/*   In   - array to run moving window over will remain umchanged                   */
/*   Ctr  - array storing results of runmed                                         */
/*   Out  - empty space for array to store the results                              */
/*   nIn  - size of arrays In and Out                                               */
/*   nWin - size of the moving window                                               */
/* Output :                                                                         */
/*   Out  - results of runing moving window over array In and colecting window mean */
/*==================================================================================*/

void runmad(double *In, double *Ctr, double *Out, const int *nIn, const int *nWin)
{ 
  int i, k, j, l, *idx2, n=*nIn, m=*nWin;
  double *Win1, *Win2, *in, *out, *ctr, med0, med;

  idx2 = Calloc(m,int   );       /* index will hold partially sorted index numbers of Save array */
  Win1 = Calloc(m,double);       /* stores all points of the current running window */
  Win2 = Calloc(m,double);       /* stores all points of the current running window */
  k    =  m/2;                   /* half of moving window size */  
  in   = In;                     /* initialize pointer to input In vector */
  out  = Out+k;                  /* initialize pointer to output Mad vector */
  ctr  = Ctr+k;                  /* initialize pointer to output Mad vector */
  med0 = 0;                      /* med0 - will save previous center (median) sowe know it changed */
  for(i=0; i<m; i++) {
    Win1[i] = Win2[i] = *(in++); /* initialize running windows */
    idx2[i] = i;                 /* and its indexes */
  }
  in--;                          /* last point of the window will be placed again */
  for(j=i=m-1; i<n; i++) {       /* the main loop */
    Win1[j] = *(in++);           /* Move Win to the right: replace a[i-m] with a[m] point  */    
    med     = *(ctr++);          /* find median of current Win1 */
    if (med==med0)               /* median did not changed */
      Win2[j]=fabs(Win1[j]-med); /* recalculate one point */
    else for(l=0; l<m; l++)      /* median did not changed */
      Win2[l]=fabs(Win1[l]-med); /* recalculate whole window Win2 */
    insertion_sort(Win2,idx2,m); /* sort Win2 - if medians did not change than  data should be sorted*/
    *(out++) = Win2[idx2[k]];    /* find mad of current Win1 and store it */
    med0 = med;                  /* save previous median */
    j = (j+1)%m;                 /* index goes from 0 to m-1, and back to 0 again  */
  }
  Free(Win2);
  Free(Win1);
  Free(idx2);
}

#ifdef DEBBUG

int main()
{
  double x[] ={1,1.123456e10,1.987654e20,-1.987654e20,-1.123456e10, -1};
  double y[6];
  double p[1024];
  int n = 6;

  cumsum_exact(x,y,&n);
  printf("%e %e %e %e %e %e\n", x[0], x[1], x[2], x[3], x[4], x[5]);
  printf("%e %e %e %e %e %e\n", y[0], y[1], y[2], y[3], y[4], y[5]);
  getchar();

  return 0;
}

#endif
