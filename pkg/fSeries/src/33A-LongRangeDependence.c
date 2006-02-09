

/* ** Cdurbin.c ************************************************************* */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


/* 
   Function to simulate a FGN  sequence, using the Durbin-Levinson 
   coefficients.  
*/

void durbinFGN(n,h,sigma,normal,output)

long *n;
double *h, *sigma, *normal, *output;

/* 
   n  -- length of desired sequence.
   d  -- coefficient
   sigma -- standard deviation of the innovations. 
   normal  -- the sequence of innovations.
   output -- the sequence being output.
*/

{
  long i,j;
  double sigma2, *acov, *vee, *phi1, *phi2;
  acov = (double *) S_alloc(*n+1,sizeof(double));
  vee  = (double *) S_alloc(*n+1,sizeof(double));
  phi1  = (double *) S_alloc(*n+1,sizeof(double));
  phi2  = (double *) S_alloc(*n+1,sizeof(double)); 
  
  
/* 
	All the vectors necessary are allocated dynamically.
*/

  sigma2 = *sigma * (*sigma)/2;

/*
	determining the autocovariance function for this particular h.
*/

  for (i = 0 ; i <= *n; i++)
    acov[i] = sigma2*(pow((double)(i+1),2*(*h)) - 2*pow((double)i,2*(*h)) + 
		      pow((double)abs(i-1),2*(*h)));

/* 
	Determining the Durbin-Levinson coefficients and the output vector, 
	recursively.
*/

  phi1[1] = acov[1]/acov[0];
  phi2[1] = phi1[1];
  vee[0] = acov[0];
  vee[1] = vee[0]*(1 - phi1[1]*phi1[1]);
  output[1] = sqrt(vee[0])*normal[1];

  for (i=2; i <= *n; i++){
    phi1[i] = acov[i];

    for (j=1; j <= i-1; j++)
      phi1[i] -= phi2[j]*acov[i-j];
    phi1[i] = phi1[i]/vee[i-1];

    vee[i] = vee[i-1]*(1-phi1[i]*phi1[i]);
    output[i] = sqrt(vee[i-1])*normal[i];
    for (j=1; j <= i-1; j++){
      phi1[j] = phi2[j]-phi1[i]*phi2[i-j];

      output[i] += phi2[j]*output[i-j];
    }
    
    for (j=1; j <= i; j++)
      phi2[j] =  phi1[j];
  }

  free(acov);
  free(vee);
  free(phi1);
  free(phi2);
}


/* *** Cfract2.c ************************************************************ */


/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */


/* 
	Written by Vadim Teverovsky 
*/


/* 
	S feeds this function a time series (data) of length n. 
*/

void
Cfractal2(data, n, nvar, v)
double *data;
long *n, *nvar;
double *v;

{
  int N = (int)*n, NVAR = (int)*nvar;

  int g, i, j, k, m, npoints;
  double increment, temp, L;

  increment = (log10((double) N/2))/NVAR;

  for(g = 1; g < NVAR + 1; g++)
    {
      k = (int)floor(pow(10.,(g * increment)));
      v[g]= 0;
      for (m = 1; m < k+1; m++)
	{
	  npoints = (int)floor(((double) N -m)/ k);
	  L = 0;
	  for(i = 1; i < npoints+1; i++){
	    temp =0.0;
	    for (j = 0; j < k; j++){
	      temp += data[m+(i-1)*k+j];}
	    L += fabs(temp);
/*	    L += fabs(data[m+i*k-1]-data[m+(i-1)*k-1]);*/
	  }
	  L = L*(N-1)/(npoints*k*k);
	  v[g] += L;
	}
      v[g] = v[g]/k;
    }
}


/* *** Cfract.c ************************************************************* */


/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */


/* 
	Written by Vadim Teverovsky. 
*/


/* 
	S feeds this function a time series (data) of length n. 
	
*/


void
Cfractal(data, n, nvar, v)
double *data;
long *n, *nvar;
double *v;

{
  int N = (int)*n, NVAR = (int)*nvar;

  int g, i, k, m, npoints;
  double increment, L;

  increment = (log10((double) N/2))/NVAR;

  for(g = 1; g < NVAR + 1; g++)
    {
      k = (int)floor(pow(10.,(g * increment)));
      v[g]= 0;
      for (m = 1; m < k+1; m++)
	{
	  npoints = (int)floor(((double) N -m)/ k);
	  L = 0;
	  for(i = 1; i < npoints+1; i++)
	    L += fabs(data[m+i*k-1]-data[m+(i-1)*k-1]);
	  L = L*(N-1)/(npoints*k*k);
	  v[g] += L;
	}
      v[g] = v[g]/k;
    }
}



/* *** Cmoments2.c ********************************************************** */


/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */


/* Written by Vadim Teverovsky */

/* S feeds this function a time series (data) of length n.  A vector of
   variances (v) is returned.  This vector has length nvar + 1 and its ith
   component is the variance of (the integer part of) n/m terms (m depends on 
   i and nvar and is defined below), each of which is the average of m terms.
 */

void
Cmoments2 (data, n, nvar, minpts, nmom, v)
     double *data, *nmom;
     long *n, *nvar, *minpts;
     double *v;

{
  double increment, grandmean, NMOM = (double) *nmom;
  int N = (int) *n, NVAR = (int) *nvar;
  int i, j, m, npoints;
  double *averages;

  double mean (), moment2 ();

  /* Dynamically allocate storage to averages. */

  averages = (double *) calloc (N, sizeof (double));
  increment = (log10 ((double) N / *minpts)) / NVAR;
  grandmean = mean (data, 0, N);
  v[0] = moment2 (data, grandmean, N, NMOM);

  for (i = 1; i < NVAR + 1; i++)
    {
      m = (int) floor (pow (10., (i * increment)));
      npoints = (int) floor ((double) N / m);
      for (j = 0; j < npoints; j++)
	averages[j] = mean (data, j, m);
      v[i] = moment2 (averages, grandmean, npoints, NMOM);
    }

/* deallocate space pointed to by averages */

  free (averages);
}

double
mean2 (data, j, m)
     double *data;
     int j, m;
{
  double sum, average;
  int i, first, last;

  sum = 0.;
  first = m * j;
  last = m * (j + 1);
  for (i = first; i < last; i++)
    sum = sum + data[i];
  average = sum / m;
  return (average);
}

double
moment2 (averages, grandmean, npoints, NMOM)
     double *averages;
     double grandmean, NMOM;
     int npoints;
{
  int i;
  double sumsq, var;

  sumsq = 0.;
  for (i = 0; i < npoints; i++)
    sumsq = sumsq + pow (fabs (averages[i]), NMOM);
  var = sumsq / (npoints);
  return (var);
}

/* eof Cmoments2.c */


/* *** Cmoments.c *********************************************************** */


/* Cmoments.c */

/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */


/* Written by Vadim Teverovsky */
/* S feeds this function a time series (data) of length n.  A vector of
   variances (v) is returned.  This vector has length nvar + 1 and its ith
   component is the variance of (the integer part of) n/m terms (m depends on 
   i and nvar and is defined below), each of which is the average of m terms.
 */

void
Cmoments (data, n, nvar, minpts, nmom, v)
     double *data, *nmom;
     long *n, *nvar, *minpts;
     double *v;

{
  double increment, grandmean, NMOM = (double) *nmom;
  int N = (int) *n, NVAR = (int) *nvar;
  int i, j, m, npoints;
  double *averages;

  double mean (), moment ();

  /* Dynamically allocate storage to averages. */

  averages = (double *) calloc (N, sizeof (double));
  increment = (log10 ((double) N / *minpts)) / NVAR;
  grandmean = mean (data, 0, N);
  v[0] = moment (data, grandmean, N, NMOM);

  for (i = 1; i < NVAR + 1; i++)
    {
      m = (int) floor (pow (10., (i * increment)));
      npoints = (int) floor ((double) N / m);
      for (j = 0; j < npoints; j++)
	averages[j] = mean (data, j, m);
      v[i] = moment (averages, grandmean, npoints, NMOM);
    }

  /* Deallocate space pointed to by averages. */

  free (averages);
}

double
mean (data, j, m)
     double *data;
     int j, m;
{
  double sum, average;
  int i, first, last;

  sum = 0.;
  first = m * j;
  last = m * (j + 1);
  for (i = first; i < last; i++)
    sum = sum + data[i];
  average = sum / m;
  return (average);
}

double
moment (averages, grandmean, npoints, NMOM)
     double *averages;
     double grandmean, NMOM;
     int npoints;
{
  int i;
  double sumsq, var;

  sumsq = 0.;
  for (i = 0; i < npoints; i++)
    sumsq = sumsq + pow (fabs (averages[i] - grandmean), NMOM);
  var = sumsq / (npoints);
  return (var);
}

/* eof Cmoments.C */


/* *** Cpeng.c ************************************************************** */


/*Written by Vadim Teverovsky*/

/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */

#define BIG 1.0e30
#define AFAC 1.5
#define AMP 1.5
double xmed;

void Cpeng(data, n, nvar, minpts, v)
double *data, *v;
long *n, *nvar, *minpts;

{
  int N = (int)*n, NVAR = (int)*nvar;

  int i, j, m, npoints;
  double increment, grandmean3;
  double mean3(), variance3(), lsfit3();
  void median3();

/* dynamically allocate storage to var*/

  double *var, *residual;
  var = (double *) calloc(N+1,sizeof(double));
  residual = (double *) calloc(N+1, sizeof(double));

  increment = (log10((double) N/3))/NVAR;
  v[0]=0;
  for(i = 1; i < NVAR + 1; i++)
    {
    v[i] = 0;
    m = 3*(int)floor(pow(10.,(i * increment)));
/*    npoints = (int)floor((double) N - m - 1);*/
    npoints = (int)floor((double) N/m);
    for ( j = 1; j <= npoints; j++){
      var[j] = lsfit3(data, j-1, m, residual);
/*      v[i] += var[j]/npoints; */
    }
    median3(var, npoints);
/*    if (npoints < 100) {*/
/*      for ( j = 1; j <= npoints; j++){*/
/*	printf("var[%d] = %f \n", j, var[j]);}*/
/*}*/
/*    printf("m = %d, npoints = %d, v[%d] = %f, med = %f\n", m,npoints,i,v[i],xmed);*/
    v[i] = xmed;


  }
/* deallocate space pointed to by var */

  free(var);
  free(residual);
}

void
median3(x, n)
int n;
double *x;
{
  int i, np, nm, j;
  double xx, xp, xm, sumx, sum , eps, stemp, dum, ap, am, aa, a;
  
  a = 0.5*(x[1]+x[n]);
  eps = fabs(x[n]-x[1]);
  am = -(ap=BIG/2);
  for(i=1;;i++){
    sum=sumx=0.0;
    np=nm=0;
    xm = -(xp=BIG/2);

    for(j = 1; j <= n; j++){
      xx = x[j];
      if ( xx != a) {
	if (xx>a){
	  ++np;
	  if(xx<xp) xp = xx;
	} else if ( xx < a) {
	  ++nm;
	  if ( xx > xm) xm = xx;
	}
	sum += dum=1.0/(eps+fabs(xx-a));
	sumx += xx*dum;
      }
    }
    stemp=(sumx/sum)-a;
/*      printf("n = %d, ap = %f, am = %f  \n", n, ap, am);*/
/*	printf("aa=%f, a = %f, np = %d, nm = %d\n", a, aa, np, nm);*/
    if (np-nm >=2) {
      am = a;
      aa = stemp < 0.0 ? xp : xp+stemp*AMP;
      if (aa>ap) aa = 0.5*(a+ap);
      eps = AFAC*fabs(aa-a);
      a = aa;
    } else if (nm - np >=2) {
      ap =a;
      aa = stemp > 0.0 ? xm : xm+stemp*AMP;
      if (aa<am) aa = 0.5*(a+am);
      eps = AFAC*fabs(aa-a);
      a = aa;
    } else {
      if ( n % 2 == 0) {
		xmed = 0.5*(np == nm ? xp+xm : np > nm ? a+xp : xm + a);
      } else {
 		xmed = np == nm ? a : np > nm ? xp : xm;
      }
      return;
    }
    if ( (((ap-am)/am <= .01) && ((ap-am)/ap <=.01)) || (i >=90)) {
      xmed = (ap+am)/2;
     /* printf("n = %d, ap = %f, am = %f, i = %d\n", n, ap, am, i); */
      return;
    }
    
  }
}


double
lsfit3(data, j, m, residual)
double *data, *residual;
int j, m;
{
  double variance3(), sumy, sumx, sumx2, sumxy,a,b, vari;
  int  k;

  sumy = 0.;
  sumx = 0.;
  sumxy = 0.;
  sumx2 = 0.;

  residual[0] = data[j*m];
/*  residual[0] = data[j];*/
  for ( k = 1; k < m; k++){
    residual[k] = residual[k-1] + data[k+j*m];
  }
  for ( k = 0; k < m; k++){  
    sumy += residual[k];
    sumx += k;
    sumx2 += ((double) k) * ((double) k);
    sumxy += k*residual[k];
  }

  if ((sumx2-sumx*sumx) == 0) return(0);

  a = (sumxy- sumx*sumy/m)/(sumx2-sumx*sumx/m);
  b = sumy/m-sumx/m*a;
 /* printf("j = %d, m = %d, a= %f, b= %f \n", j, m, a, b);*/
  for ( k = 0; k < m; k++){
    residual[k] -= a*k+b;
  }
  vari = variance3(residual, m);
  return(vari); 
}

double
mean3(data, j, m)
double *data;
int j, m;
{
  double sum, average;
  int i, first, last;

  sum = 0.;
  first = m * j;
  last = m * (j + 1);
  for(i = first; i < last; i++)
    sum = sum + data[i];
  average = sum / m;
  return(average);
}

double
variance3(averages, npoints)
double *averages;
int npoints;
{
  int i;
  double sumsq, var, ave, mean3();

  ave = mean3(averages, 0,npoints);
  sumsq = 0.;
  for(i = 0; i < npoints; i++)
    sumsq = sumsq + pow(averages[i]-ave, 2.);
  var = sumsq/(npoints-1);
  return(var);
}


/* *** Cpengav.c ************************************************************ */


/*Written by Vadim Teverovsky*/

/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */

#define BIG 1.0e30
#define AFAC 1.5
#define AMP 1.5


void Cpengav(data, n, nvar, minpts, v)
double *data, *v;
long *n, *nvar, *minpts;

{
  int N = (int)*n, NVAR = (int)*nvar;

  int i, j, m, npoints;
  double increment, grandmean4, *xmed;
  double mean4(), variance4(), lsfit4();
  void median4();

/* dynamically allocate storage to var*/

  double *var, *residual;
  var = (double *) calloc(N+1,sizeof(double));
  residual = (double *) calloc(N+1, sizeof(double));

  increment = (log10((double) N/3))/NVAR;
  v[0]=0;
  for(i = 1; i < NVAR + 1; i++)
    {
    v[i] = 0;
    m = 3*(int)floor(pow(10.,(i * increment)));
    npoints = (int)floor((double) N / m);
    for ( j = 1; j <= npoints; j++){
      var[j] = lsfit4(data, j-1, m, residual);
      v[i] += var[j]/npoints; 
/*      if (i > 37 ){  */
/*      printf("var[%d] = %f, npoints = %d, m = %d v[%d]= %f\n",j,var[j],npoints, m,i,v[i]);}*/
/*    } */
/*    median4(var,npoints,xmed);*/
/*    if (npoints < 100) {*/
/*      for ( j = 1; j <= npoints; j++){*/
/*	printf("var[%d] = %f \n", j, var[j]);}}*/
/*    v[i] = *xmed;*/
/*    printf("m = %d, npoints = %d v[%d] = %f\n", m,npoints,i,v[i]);*/

  }
  
/* deallocate space pointed to by var */

  free(var);
  free(residual);
  }
}

void
median4(x, n, xmed)
int n;
double x[], *xmed;
{
  int i, np, nm, j;
  double xx, xp, xm, sumx, sum , eps, stemp, dum, ap, am, aa, a;
  
  a = 0.5*(x[1]+x[n]);
  eps = fabs(x[n]-x[1]);
  am = -(ap=BIG/2);
  for(i=1;;i++){
    sum=sumx=0.0;
    np=nm=0;
    xm = -(xp=BIG/2);

    for(j = 1; j <= n; j++){
      xx = x[j];
      if ( xx != a) {
	if (xx>a){
	  ++np;
	  if(xx<xp) xp = xx;
	} else if ( xx < a) {
	  ++nm;
	  if ( xx > xm) xm = xx;
	}
	sum += dum=1.0/(eps+fabs(xx-a));
	sumx += xx*dum;
      }
    }

    stemp=(sumx/sum)-a;
/*      printf("n = %d, ap = %f, am = %f  \n", n, ap, am);*/
/*	printf("aa=%f, a = %f, np = %d, nm = %d\n", a, aa, np, nm);*/
    if (np-nm >=2) {
      am = a;
      aa = stemp < 0.0 ? xp : xp+stemp*AMP;
      if (aa>ap) aa = 0.5*(a+ap);
      eps = AFAC*fabs(aa-a);
      a = aa;
    } else if (nm - np >=2) {
      ap =a;
      aa = stemp > 0.0 ? xm : xm+stemp*AMP;
      if (aa<am) aa = 0.5*(a+am);
      eps = AFAC*fabs(aa-a);
      a = aa;
    } else {
      if ( n % 2 == 0) {
	*xmed = 0.5*(np == nm ? xp+xm : np > nm ? a+xp : xm + a);
      } else {
 	*xmed = np == nm ? a : np > nm ? xp : xm;
/*      printf("in third2\n");*/
      }
      return;
    }
    if ( (((ap-am)/am <= .01) && ((ap-am)/ap <=.01)) || (i >=30)) {
      *xmed = (ap+am)/2;

      /* printf("n = %d, ap = %f, am = %f, i = %d\n", n, ap, am, i); */
      return;
    }
    
  }
}


double
lsfit4(data, j, m, residual)
double *data, *residual;
int j, m;
{
  double variance4(), sumy, sumx, sumx2, sumxy,a,b, vari;
  int  k;

  sumy = 0.;
  sumx = 0.;
  sumxy = 0.;
  sumx2 = 0.;

  residual[0] = data[j*m];
  for ( k = 1; k < m; k++){
    residual[k] = residual[k-1] + data[k+j*m];
  }
  for ( k = 0; k < m; k++){  
    sumy += residual[k];
    sumx += k;
    sumx2 += ((double) k) * ((double) k);
    sumxy += k*residual[k];
  }

  if ((sumx2-sumx*sumx) == 0) return(0);

  a = (sumxy- sumx*sumy/m)/(sumx2-sumx*sumx/m);
  b = sumy/m-sumx/m*a;
 /* printf("j = %d, m = %d, a= %f, b= %f \n", j, m, a, b);*/
  for ( k = 0; k < m; k++){
    residual[k] -= a*k+b;
  }
  vari = variance4(residual, m);
  return(vari); 
}

double
mean4(data, j, m)
double *data;
int j, m;
{
  double sum, average;
  int i, first, last;

  sum = 0.;
  first = m * j;
  last = m * (j + 1);
  for(i = first; i < last; i++)
    sum = sum + data[i];
  average = sum / m;
  return(average);
}

double
variance4(averages, npoints)
double *averages;
int npoints;
{
  int i;
  double sumsq, var, ave, mean4();

  ave = mean4(averages, 0,npoints);
  sumsq = 0.;
  for(i = 0; i < npoints; i++)
    sumsq = sumsq + pow(averages[i]-ave, 2.);
  var = sumsq/(npoints-1);
  return(var);
}


/* *** Crs.c **************************************************************** */


/* #include <math.h> */
/* #include <stdio.h> */
/* #include <stdlib.h> */

/*Written by Bob Sherman, modified by Walter Willinger, Vadim Teverovsky.*/


/*
 #define  r(i,j)       output[(i) * NBLK + (j)]
 #define  radj(i,j)    output[NBLK * NLAG + (i) * NBLK + (j)] 
 removed by DW 20/9/2003 */

/* *** Crs
 * S feeds this function a time series (data) of length n.
 * The appropriate r and r/s statistics are computed and then
 * returned to S through the vector output.
 */

void
Crs (data, n, nblk, nlag, overlap, output)
     double *data;
     long *n, *nblk, *nlag, *overlap;
     double *output;

{
  int N = (int) *n, NBLK = (int) *nblk, NLAG = (int) *nlag;
  int OVERLAP = (int) *overlap, BLKSIZE;

  int i, j, k, d, correction, NVAL;

  double increment;
  double temp, min, max, s;
  double ave, secondmom;

/* Dynamically allocate storage to xcum and xsqcum. */

  double *xcum, *xsqcum;

  xcum = (double *) calloc (N, sizeof (double));
  xsqcum = (double *) calloc (N, sizeof (double));

#if defined (DEBUG)
  printf("you made it!\n\n");
#endif

  /* Compute xcum's and xsqcum's. */

  xcum[0] = data[0];
  xsqcum[0] = data[0] * data[0];

  for (i = 1; i < N; i++)
    {
      xcum[i] = xcum[i - 1] + data[i];
      xsqcum[i] = xsqcum[i - 1] + data[i] * data[i];
    }

  /* Compute r and radj. */

  BLKSIZE = (int) floor (((double) N) / ((double) NBLK));

  if (OVERLAP != 0)
    increment = (log10 ((double) N)) / NLAG;
  else
    increment = (log10 ((double) BLKSIZE)) / NLAG;

  for (k = 0; k < NLAG; k++)
    {
      if (k == NLAG - 1)
	d = (int) (pow (10., (double) (increment * (k + 1))));
      else
	d = (int) ceil (pow (10., (double) (increment * (k + 1))));

      /* d observations used to compute r and radj for lag k. */

      correction = (int) ceil (((double) (d - BLKSIZE)) / ((double) BLKSIZE));
      if (correction == NBLK)
	correction = correction - 1;
      else;

      if (d > BLKSIZE)
	NVAL = NBLK - correction;
      else
	NVAL = NBLK;

#if defined (DEBUG)
      printf("NVAL[%d] = %d  d[%d] = %d\n",k + 1,NVAL,k + 1,d);
#endif

      /* NVAL is the number of r and radj values computed for lag k. */

      /* i = 0 is a special case. */

      max = min = 0.;
      ave = (1. / d) * xcum[d - 1];
      for (j = 0; j < d; j++)
	{
	  temp = xcum[j] - (j + 1) * ave;
	  if (temp > max)
	    max = temp;
	  else if (temp < min)
	    min = temp;
	}
      output[k * NBLK + 0] = max - min;  /* r (k, 0) = max - min; */
      secondmom = (1. / d) * xsqcum[d - 1];
      if (secondmom > ave * ave)
	{
	  s = sqrt (secondmom - ave * ave);
	  output[NBLK * NLAG + k * NBLK + 0] = output[k * NBLK + 0] / s;  /* radj (k, 0) = r (k, 0) / s; */
	}
      else
	{
	  output[NBLK * NLAG + k * NBLK + 0] = output[k * NBLK + 0] ;  /* radj (k, 0) = r (k, 0); */
#if defined (DEBUG)
	  printf("s[%d][1] = 0 radj(%d, 1) set equal to r(%d, 1)\n", k+1, k+1, k+1);
#endif
	}

      /* i > 0 */

      for (i = 1; i < NVAL; i++)
	{
	  max = min = 0.;
	  ave = (1. / d) * (xcum[BLKSIZE * i - 1 + d] - xcum[BLKSIZE * i - 1]);
	  for (j = 0; j < d; j++)
	    {
	      temp = xcum[BLKSIZE * i + j] - xcum[BLKSIZE * i - 1]
		- (j + 1) * ave;
	      if (temp > max)
		max = temp;
	      else if (temp < min)
		min = temp;
	    }
	  output[k * NBLK + i] = max - min;  /* r (k, i) = max - min; */
	  secondmom = (1. / d) * (xsqcum[BLKSIZE * i - 1 + d]
				  - xsqcum[BLKSIZE * i - 1]);
	  if (secondmom > ave * ave)
	    {
	      s = sqrt (secondmom - ave * ave);
	      output[NBLK * NLAG + k * NBLK + i] = output[k * NBLK + i] / s; /*radj (k, i) = r (k, i) / s; */
	    }
	  else
	    {
		output[NBLK * NLAG + k * NBLK + i] = output[k * NBLK + i]; /* radj (k, i) = r (k, i); */
#if defined (DEBUG)
	      printf("s[%d][%d] = 0 radj(%d, %d) set equal to r(%d, %d)\n", k+1, i+1, k+1, i+1, k+1, i+1);
#endif
	    }
	}
    }

  /* Deallocate space pointed to by xcum and xsqcum. */

  free (xcum);
  free (xsqcum);

}

/* eof Crs.c */


/* *** Cvariances.c ********************************************************* */


/* Cvariances.c */
/* Written by Bob Sherman, modified by Walter Willinger, Vadim Teverovsky.*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* S feeds this function a time series (data) of length n.  A vector of
   variances (v) is returned.  This vector has length nvar + 1 and its ith
   component is the variance of (the integer part of) n/m terms (m depends on 
   i and nvar and is defined below), each of which is the average of m terms.
 */

void
Cvariances (data, n, nvar, minpts, v)
     double *data;
     long *n, *nvar, *minpts;
     double *v;

{
  int N = (int) *n, NVAR = (int) *nvar;
  double increment, grandmean;
  int i, j, m, npoints;
  double *averages;

  double mean1 (), variance1 ();

  /* Dynamically allocate storage to averages. */

  averages = (double *) calloc (N, sizeof (double));

  increment = (log10 ((double) N / *minpts)) / NVAR;
  grandmean = mean1 (data, 0, N);
  v[0] = variance1 (data, grandmean, N);

  for (i = 1; i < NVAR + 1; i++)
    {
      m = (int) floor (pow (10., (i * increment)));
      npoints = (int) floor ((double) N / m);
      for (j = 0; j < npoints; j++)
	averages[j] = mean1 (data, j, m);
      v[i] = variance1 (averages, grandmean, npoints);
    }

  /* Deallocate space pointed to by averages. */

  free (averages);
}

double
mean1 (data, j, m)
     double *data;
     int j, m;
{
  double sum, average;
  int i, first, last;

  sum = 0.;
  first = m * j;
  last = m * (j + 1);
  for (i = first; i < last; i++)
    sum = sum + data[i];
  average = sum / m;
  return (average);
}


double
variance1 (averages, grandmean, npoints)
     double *averages;
     double grandmean;
     int npoints;
{
  int i;
  double sumsq, var;

  sumsq = 0.;
  for (i = 0; i < npoints; i++)
    sumsq = sumsq + pow (averages[i] - grandmean, 2.);
  var = sumsq / (npoints - 1);
  return (var);
}

/* eof Cvariances. */


/* ************************************************************************** */

