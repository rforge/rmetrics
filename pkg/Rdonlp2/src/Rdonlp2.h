/***********************************************************************

    Rdonlp2.h - 

    Copyright (C) 2007 Ryuichi Tamura (ry.tamura @ gmail.com)

 ***********************************************************************/
#include <R.h>
#include <Rinternals.h>

/* donlp2() */
extern void donlp2(void);

/* teardown */
extern void global_mem_free(void);

/* user_init_size() */
extern void user_init_size(void);
extern int n,nlin,nonlin,iterma,nstep;

/* user_init() */
extern void user_init(void);
extern int bloc,analyt,difftype,fsilent,silent,intakt,cold;
extern char name[41];
extern double *x,*low,*up,**gres;
extern double tau0,tau,del0,epsx,delmin,epsdif,epsfcn,taubnd;

/* setup() */
extern void setup(void);
extern int nreset,te0, te1, te2, te3, calc_hessian;

/* ef() */
extern void ef(double *, double *);
extern int ffuerr;

/* egradf() */
extern void egradf(double *, double *);
extern FILE *prou, *meu;

/* econ() */
extern void econ(int, int *, double *, double *, int *);
extern int *confuerr;

/* econgrad() */
extern void econgrad(int *, int, double *, double **);
/* extern FILE *prou, *meu; */

/* newx() */
extern void newx(double *, double *, int, double **, int *);
extern double *w, *gradf, **accinf;

/* solchk() */
extern void solchk(void);
extern int itstep;
extern double /* **accinf,*/ *x, /* *gradf,*/ *u, /* *w,*/ **a, **hess;
extern float optite,runtim;
extern double fx;
/* extern FILE *prou, *meu; */


/*** the structure passing information from R function to donlp() ***/
typedef struct {
  /*** Rdonlp2 -> user_init_size() ***/
  int n;
  int nlin;
  int nonlin;

  /*** Rdonlp2 -> user_init() ***/
  char name[41];
  double *x; /* length: n*/

  /* upper and lower bounds for constraints */
  double *low; /* length: nres */
  double *up;  /* length: nres */
  /* R matrix (dim: n x nlin ) that represents linear constraints */
  double *gres;

  /*** control variables ***/

  /* setup parameters*/
  int iterma;
  int nstep;
  double fnscale;
  int do_report;
  int rep_freq;
  int analyt;
  
  /* performance and tunings */
  double tau0;
  double tau;
  double del0;

  /* termination criteria */
  double epsx;
  double delmin;
  double epsdif;
  int nreset_multiplier;

  /* numerical differentiation */
  int difftype;
  double epsfcn;
  double taubnd;
  int calc_hessian;
  
  /* information */
  int te0;
  int te1;
  int te2;
  int te3;
  int silent;
  int fsilent;
  int intakt;
  
  /*** function and environments ***/
  SEXP env;
  SEXP fn;
  SEXP accfn;
  SEXP accinf;
  
  /*** return value ***/
  SEXP ret;
  
} Rdonlp2Info;

static Rdonlp2Info *info;
