/***********************************************************************

    Rdonlp2.c - R extension library for DONLP2

    Copyright (C) 2007 Ryuichi Tamura (ry.tamura @ gmail.com)

 ***********************************************************************/
#include "Rdonlp2.h"

/*** list reader function ***/
static SEXP
getListElement(SEXP list, char *str)
{
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  int i;
  for (i = 0; i < length(list); i++){
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}

/*** initializer for Rdonlp2Info ***/
static Rdonlp2Info*
init_Rdonlp2Info(int npar, int nlin, int nonlin)
{
  int i;
  Rdonlp2Info *ret = (Rdonlp2Info *)R_alloc(1, sizeof(Rdonlp2Info));
  
  ret->n = npar;
  ret->nlin = nlin;
  ret->nonlin = nonlin;

  /* allocate arrays */
  ret->x   = (double *)R_alloc(npar, sizeof(double));
  for (i=0; i<npar; i++) ret->x[i] = 0.0;
  ret->low = (double *)R_alloc(npar+nlin+nonlin, sizeof(double));
  ret->up  = (double *)R_alloc(npar+nlin+nonlin, sizeof(double));
  for (i=0; i<npar+nlin+nonlin; i++) ret->low[i]=ret->up[i]=0.0;
  ret->gres = (double *)R_alloc(npar*nlin, sizeof(double));
  for (i=0; i<npar*nlin; i++) ret->gres[i]=0.0;

  memset(ret->name, '\0', 41);
  return ret;
}

/*** copy bounds parameters (par, lin, nlin)             ***/
/*** 'big' must be the same value as defined in donlp2() ***/
static void
copy_bounds(Rdonlp2Info *info, SEXP lbd, SEXP ubd)
{
  double big;
  int i, len;
  double vl, vu;

  if (length(lbd)!=length(ubd)){
    error("call_donlp2: length of bounds differ (%d != %d)",
	  length(lbd), length(ubd));
  }
  len = length(lbd);
  big = 1.0e20;
  
  for (i=0; i<len; i++){
    vl = REAL(lbd)[i]; vu = REAL(ubd)[i];
    if (R_FINITE(vl))
      info->low[i] = vl;
    else
      info->low[i] = -big;

    if (R_FINITE(vu))
      info->up[i] = vu;
    else
      info->up[i] = big;
  }
  return;
}

/*** 'main' function ***/
SEXP
call_donlp2(SEXP par,            /* initial parameter vector        */
	    SEXP num_lin,        /* number of linear constraints    */
	    SEXP num_nonlin,     /* number of nonlinear constraints */
	    SEXP fsilent,        /* output to .[mes|pro] file ?     */
	    SEXP rident,         /* identifier of the optimization  */
	    SEXP num_rident,     /* number of characters in identifier */
	    SEXP lbd, SEXP ubd,  /* bounds for constraint: par->nlin->nonlin */
	    SEXP A,              /* coef matrix for lin constraint */
	    SEXP control,        /* list of control variables */
	    SEXP accfun,         /* parses accumlated information */
	    SEXP confun,SEXP Renv/* evaluation context  */  )
{
  int i;
  int npar, len;

  npar = length(par); len = asInteger(num_rident);
  /* initialize */
  info = (Rdonlp2Info *)NULL;
  info = init_Rdonlp2Info(npar, asInteger(num_lin), asInteger(num_nonlin));
  
  /* copy values */
  if (!asLogical(fsilent)){
    if (len<40){
      strcpy(info->name, CHAR(STRING_ELT(rident,0)));
    }
    else {
      strncpy(info->name, CHAR(STRING_ELT(rident,0)), 40);
    }
  }

  for (i=0; i<info->n; i++){
    info->x[i] = REAL(par)[i];
  }

  copy_bounds(info, lbd, ubd);

  for (i=0; i<info->n*info->nlin; i++){
    info->gres[i] = REAL(A)[i];
  }

  /*** copy control variables ***/
  /* setup parameters*/
  info->iterma    = asInteger(getListElement(control, "iterma"));  
  info->nstep     = asInteger(getListElement(control, "nstep"));
  info->fnscale   = asReal(getListElement(control, "fnscale"));  
  info->do_report = asLogical(getListElement(control, "report"));
  info->rep_freq  = asInteger(getListElement(control, "rep.freq"));
  info->analyt    = asLogical(getListElement(control, "analyt"));

  /* performance and tunings */
  info->tau0 = asReal(getListElement(control, "tau0"));
  info->tau  = asReal(getListElement(control, "tau"));
  info->del0 = asReal(getListElement(control, "del0"));

  /* termination criteria */
  info->epsx      = asReal(getListElement(control, "epsx"));
  info->delmin    = asReal(getListElement(control, "delmin"));
  info->taubnd    = asReal(getListElement(control, "taubnd"));
  info->nreset_multiplier =
    asInteger(getListElement(control, "nreset.multiplier"));

  /* numerical differentiation */
  info->difftype = asInteger(getListElement(control, "difftype"));
  info->epsdif   = asReal(getListElement(control, "epsdif"));
  info->epsfcn   = asReal(getListElement(control, "epsfcn"));
  info->calc_hessian = asLogical(getListElement(control, "hessian"));
  
  /* information */
  info->te0    = asLogical(getListElement(control, "te0"));
  info->te1    = asLogical(getListElement(control, "te1"));
  info->te2    = asLogical(getListElement(control, "te2"));
  info->te3    = asLogical(getListElement(control, "te3"));
  info->silent = asLogical(getListElement(control, "silent"));
  info->fsilent = asLogical(fsilent);
  info->intakt = asLogical(getListElement(control, "intakt"));

  /*  evaluation function and env */
  if (!isFunction(confun))error("confun must be a function");
  if (!isEnvironment(Renv))error("Renv must be an environment");
  if (!isFunction(accfun))error("confun must be a function");

  info->fn = confun;
  info->env = Renv;
  info->accfn = accfun;

  /* start optimization */
  donlp2();

  /* return the result as (duplicated) list object: Raccinf */
  return(duplicate(info->ret));
}

SEXP
teardown(SEXP dummy)
{
  if (meu) {fclose(meu); meu=(FILE *)NULL;}
  if (prou){fclose(prou); prou=(FILE *)NULL;}

  global_mem_free();
  return R_NilValue;
}

void
user_init_size()
{
  n      = info->n;
  nlin   = info->nlin;
  nonlin = info->nonlin;
  iterma = info->iterma;
  nstep  = info->nstep;
  /* calc_hessian must be set in this function because */
  /* global_mem_malloc() allocates memory for 'hess' */
  calc_hessian = info->calc_hessian;

  return;
}

void
user_init()
{
  int i,j;
  
  bloc = FALSE;

  
  /* copy parameters */
  for (i=1; i<=info->n; i++){
    x[i] = info->x[i-1];
  }

  /* copy bounds */
  for (i=1; i<=info->n+info->nlin+info->nonlin; i++){
    low[i] = info->low[i-1];
    up[i]  = info->up[i-1];
  }

  /* coefs for linear constraints */
  for (i=1; i<=info->nlin; i++){
    for (j=1; j<=info->n; j++){
      gres[j][i] = info->gres[(i-1)*info->n + (j-1)];
    }
  }

  /* analyt */
  analyt = info->analyt;
  /* performance and tunings */
  tau0 = info->tau0; tau = info->tau; del0 = info->del0;

  /* termination criteria */
  epsx = info->epsx; delmin = info->delmin; epsdif=info->epsdif;

  /* numerical differentiation */
  difftype = info->difftype;
  epsfcn = info->epsfcn;
  taubnd = info->taubnd;  

  /* information */
  silent = info->silent;
  fsilent = TRUE;
  if (!info->fsilent){
    fsilent = FALSE;
    strcpy(name, info->name);
  }
  intakt = info->intakt;

  /* cold */
  cold = TRUE;
  return;
}

void
setup()
{
  /* nreset : good values in [n, 3*n] */
  nreset = info->n*info->nreset_multiplier;

  if (info->te3){
    te3 = te2 = te1 = te0 = TRUE;
  }
  else if (info->te2){
    te2 = te1 = te0 = TRUE;
  }
  else if (info->te1){
    te1 = te0 = TRUE;
  }
  else {
    te0 = info->te0;
  }

  /* finally check fnscale != 0 */
  if (info->fnscale == 0.0)
    error("fnscale must not be zero.");
  return;
}
 
void
ef(double x[], double *fx)
{
  int i, n;
  SEXP ans, tmp_x;
  
  n = info->n;
  PROTECT(tmp_x = allocVector(REALSXP, 1+1+n));
  REAL(tmp_x)[0] = 0; /* function value */
  REAL(tmp_x)[1] = 0; /* objective function */
  for (i=2; i<n+2; i++){
    REAL(tmp_x)[i] = x[i-1];
  }
  PROTECT(ans = eval(lang2(info->fn, tmp_x), info->env));
  if (length(ans)>1){
    ffuerr = REAL(ans)[1]>0 ? TRUE : FALSE;
  }
  *fx = REAL(ans)[0]/info->fnscale;
  UNPROTECT(2);
  
  return;
}

void
egradf(double x[], double gradf[])
{
  int i, n;
  SEXP ans, tmp_x;

  n = info->n;
  PROTECT(tmp_x = allocVector(REALSXP, 1+1+n));
  REAL(tmp_x)[0] = 1; /* gradient values */
  REAL(tmp_x)[1] = 0; /* objective function */
  for (i=2; i<n+2; i++){
    REAL(tmp_x)[i] = x[i-1];
  }
  PROTECT(ans = eval(lang2(info->fn, tmp_x), info->env));
  PROTECT(ans = coerceVector(ans, REALSXP));
  if (length(ans)!=n){
    error("fn: # of elements of gradient must equal to %d",n);
  }
  for (i=0; i<n; i++){
    gradf[i+1] = REAL(ans)[i]/info->fnscale;
  }
  
  UNPROTECT(3);
  return;
}

void
econ(int type, int liste[], double x[], double con[], int err[])
{
  int i, n;
  SEXP ans, tmp_x;

  n = info->n;

  PROTECT(tmp_x = allocVector(REALSXP, 1+1+n));
  REAL(tmp_x)[0] = 0; /* function value */
  for (i=2; i<n+2; i++){
    REAL(tmp_x)[i] = x[i-1];
  }
  
  switch (type){
  case 1: /* all constraints are evaluated */
    for (i=1; i<=info->nonlin; i++){
      REAL(tmp_x)[1] = i;
      PROTECT(ans = eval(lang2(info->fn, tmp_x), info->env));
      if (length(ans)>1){
	confuerr[i] = REAL(ans)[1]>0 ? TRUE : FALSE;
      }
      con[i] = REAL(ans)[0];
      UNPROTECT(1);
    }
    break;
  case 2: /* subset of constraints are evaluated */
    for (i=1; i<=liste[0]; i++){
      REAL(tmp_x)[1] = liste[i];
      PROTECT(ans = eval(lang2(info->fn, tmp_x), info->env));
      if (length(ans)>1){
	confuerr[i] = REAL(ans)[1]>0 ? TRUE : FALSE;
      }
      con[liste[i]] = REAL(ans)[0];
      UNPROTECT(1);
    }
    break;
  default: /* should not be reached */
    error("Rdonlp2 internal error: unknown 'type' given in econ()");
  }

  UNPROTECT(1);
  return;
}

void
econgrad(int liste[], int shift, double x[], double **grad)
{
  int i, j, n;
  SEXP ans, tmp_x;

  n = info->n;
  PROTECT(tmp_x = allocVector(REALSXP, 1+1+n));
  REAL(tmp_x)[0] = 1; /* gradient values */
  for (i=2; i<n+2; i++){
    REAL(tmp_x)[i] = x[i-1];
  }
  for (i=1; i<=liste[0]; i++){
    REAL(tmp_x)[1] = liste[i];
    PROTECT(ans = eval(lang2(info->fn, tmp_x), info->env));
    PROTECT(ans = coerceVector(ans, REALSXP));
    if (length(ans)!=n){
      error("constraint #%d: length of gradient vector must equal to %d",
	    liste[i],n);
    }
    for (j=0; j<n; j++){
      grad[j+1][liste[i]+shift] = REAL(ans)[j];
    }
    UNPROTECT(2);
  }

  UNPROTECT(1);
  return;
}


/*** we do not use eval_extern() ***/
void user_eval(double xvar[],int mode) {return;}
void eval_extern(int mode){return;}

static char *tagname[] = {
  /* 0-3 */
  "par", "gradf", "u", "w",

  /* 4-35 : accinf  */
  "step.nr",  "fx",  "scf",  "psi",  "upsi",  "del.k.1",  "b2n0",
  "b2n",  "nr",  "sing",  "umin",  "not.used",  "cond.r",  "cond.h",
  "scf0",  "xnorm",  "dnorm",  "phase",  "c.k",  "wmax",  "sig.k",
  "cfincr",  "dirder",  "dscal",  "cosphi",  "violis",  "hesstype",
  "modbfgs",  "modnr",  "qpterm",  "tauqp",  "infeas",

  /* 36-38 */
  "nr.update", "hessian", "message", "runtime"
};


static SEXP
init_Raccinf(int it, double **accinf,
	     double *x, double *gradf, double *u, double *w,
	     double **a, double **hess, char *line, double rt)
{
  int i, j, n, nlin, nonlin;
  SEXP Raccinf, Rpar, Rgrad, Ru, Rw, Rnrupd, Rhess, Rmes, Rtagname, Rruntime;
  SEXP tmp;

  n = info->n; nlin = info->nlin; nonlin = info->nonlin;
  
  /* allocate R objects */
  PROTECT(Raccinf = allocVector(VECSXP, 4+32+4));
  PROTECT(Rpar = allocVector(REALSXP,n));
  if (x){
    for (i=0; i<n; i++){
      REAL(Rpar)[i] = x[i+1];
    }
  }
  PROTECT(Rgrad = allocVector(REALSXP,n));
  if (gradf){
    for (i=0; i<n; i++){
      REAL(Rgrad)[i] = gradf[i+1]/info->fnscale;
    }
  }
  PROTECT(Ru = allocVector(REALSXP,2*(n+nlin+nonlin)));
  if (u){
    for (i=0; i<2*(n+nlin+nonlin); i++){
      REAL(Ru)[i] = u[i+1];
    }
  }
  PROTECT(Rw = allocVector(REALSXP,2*(n+nlin+nonlin)));
  if (w){
    for (i=0; i<2*(n+nlin+nonlin); i++){
      REAL(Rw)[i] = w[i+1];
    }
  }
  PROTECT(Rnrupd = allocVector(REALSXP, n*n));
  if (a){
    int p = 0;    
    for (i=1; i<=n; i++){
      for (j=1; j<=n; j++){
	REAL(Rnrupd)[p++] = a[j][i]/info->fnscale;
      }
    }
  }
  PROTECT(Rhess = allocVector(REALSXP, n*n));
  if (hess){
    int p = 0;
    for (i=1; i<=n; i++){
      for (j=1; j<=n; j++){
	REAL(Rhess)[p++] = hess[j][i]/info->fnscale;
      }
    }
  }
  PROTECT(Rmes  = allocVector(STRSXP, 1));
  if (line){
    PROTECT(tmp = mkChar(line));
    SET_STRING_ELT(Rmes, 0, tmp);
    UNPROTECT(1);
  }
  
  SET_VECTOR_ELT(Raccinf, 0, Rpar);
  SET_VECTOR_ELT(Raccinf, 1, Rgrad);
  SET_VECTOR_ELT(Raccinf, 2, Ru);
  SET_VECTOR_ELT(Raccinf, 3, Rw);
  SET_VECTOR_ELT(Raccinf, 36, Rnrupd);
  SET_VECTOR_ELT(Raccinf, 37, Rhess);
  SET_VECTOR_ELT(Raccinf, 38, Rmes);

  PROTECT(Rruntime = ScalarReal(rt));
  SET_VECTOR_ELT(Raccinf, 39, Rruntime);
  UNPROTECT(1);

  PROTECT(Rtagname = allocVector(STRSXP,40)); /*4+32+4*/
  for (i=0; i<40; i++){
    SET_VECTOR_ELT(Rtagname, i, mkChar(tagname[i]));
  }
  setAttrib(Raccinf, R_NamesSymbol, Rtagname);

  if (accinf){
    for (i = 0; i<32; i++){
      PROTECT(tmp = ScalarReal(accinf[it][i+1]));
      SET_VECTOR_ELT(Raccinf, i+4, tmp);
      UNPROTECT(1);
    }
  }

  UNPROTECT(9);
  return(Raccinf);
}

/*** donlp() calls newx() on every end of iteration ***/
void 
newx(double x[], double u[], int itstep, double **accinf, int *cont)
{
  if (info->do_report){
    SEXP ans, Raccinf;
    if ((itstep == 1) | (itstep % info->rep_freq == 0)){
      PROTECT(Raccinf = init_Raccinf(itstep, accinf, x, gradf, u, w,
				     (double **)NULL, (double **)NULL,
				     (char *)NULL, 0.0));
      PROTECT(ans = eval(lang2(info->accfn, Raccinf), info->env));
      *cont = INTEGER(ans)[0];
      UNPROTECT(2);
      return;
    }
  }
  *cont = TRUE;
  return;
}

/* termination messages taken from the local variable defined in o8fin() */
static char *messag[] = {
  "",     /* not used : index 0 */
  "constraint evaluation returns error with current point",
  "objective evaluation returns error with current point",
  "QPsolver: working set singular in dual extended QP ",
  "QPsolver: extended QP-problem seemingly infeasible ",
  "QPsolver: no descent direction from QP for tau=tau_max",
  "QPsolver: on exit correction small, infeasible point",
  "stepsizeselection: computed d not a direction of descent",
  "more than MAXIT iteration steps",
  "stepsizeselection: no acceptable stepsize in [sigsm,sigla]",
  "stepsizeselection: directional deriv. very small, infeasible",
  "KT-conditions satisfied, no further correction computed",
  "KT-conditions satisfied, computed correction small",
  "stepsizeselection: x (almost) feasible, dir. deriv. very small",
  "KT-conditions (relaxed) satisfied, singular point",
  "very slow primal progress, singular or illconditoned problem",
  "very slow progress in x, singular problem",
  "correction very small, almost feasible but singular point",
  "max(n,10) small differences in penalty function,terminate",
  "user required termination                                "
};

/*** donlp() calls solchk() on termination ***/
void
solchk()
{
  int k;
  SEXP tmp;
  char line[65];
  memset(line, '\0', 65);

  /* termination message */
  k = (int)optite+11;
  if (k >=1 && k <= 19){
    strcpy(line,messag[k]);
  }
  else {
    strcpy(line,"variable optite undefined on exit");
  }
  
  if (itstep==0){
    error("call_donlp2(): donlp2 terminates without any iterations");
  }
  /* NOTE: In o8fin(), 'itstep = itstep -1' is stated *after*
     solchk() is invoked. */
  PROTECT(info->ret = init_Raccinf(itstep-1, accinf, x, gradf, u, w,
				   a, hess, line, runtim));
  PROTECT(tmp = ScalarReal(fx/info->fnscale));
  SET_VECTOR_ELT(info->ret, 5, tmp);
  UNPROTECT(2);
  return;
}
