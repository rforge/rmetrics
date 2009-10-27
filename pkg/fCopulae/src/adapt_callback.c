#include "S.h"
#include "Rinternals.h"

/* Added declaration of FORTRAN by Yohan Chalabi  */
void F77_NAME(adapt)(int*,    /* ndim */     
		     double*, /* lower */    
		     double*, /* upper */    
		     int*,    /* minpts */   
		     int*,    /* maxpts */   
		     double*, /* eps */      
		     double*, /* relerr */   
		     int*,    /* lenwrk */   
		     double*, /* wrkstr */   
		     double*, /* finest */   
		     int*);   /* ifail */    

static SEXP rho;
static SEXP f;

/* All this routine does is call the approriate fortran
   function.  We need this so as to properly pass the S function */
/* changed to doubles for R by Thomas Lumley */
void cadapt(int *ndim, double *lower, double *upper,
	    int *minpts, int *maxpts,
	    void *functn, void *env,
	    double *eps, double *relerr,
	    int *lenwrk, double *finest, int *ifail)
{
  double *wrkstr;

  wrkstr = (double *) S_alloc(*lenwrk, sizeof(double));

  /* store the R function and its environment */
  rho=env;
  f=functn;

  F77_CALL(adapt)(ndim,lower,upper,minpts,maxpts,eps,relerr,lenwrk,
		  wrkstr,finest,ifail);
}

/* This is the fixed routine called by adapt */
/* changed to double for R, also rewritten to use eval() */

double F77_NAME(adphlp)(int *ndim, double *z)
{
  SEXP args,resultsxp,callsxp;
  double result;
  int i;

  PROTECT(args=allocVector(REALSXP,*ndim));
  for (i=0;i<*ndim;i++){
    REAL(args)[i]=z[i];
  }

  PROTECT(callsxp=lang2( f,args));
  PROTECT(resultsxp=eval(callsxp,rho));

  result=REAL(resultsxp)[0];

  UNPROTECT(3);

  return(result);
}
