/* **************************************************************************** */
/*                                  o8comm.h                                    */
/* **************************************************************************** */

/* fmin -> o8fmin */
/* dnorm -> o8dnorm */

#include "o8fuco.h"
    
X float      runtim;
X float      optite;
X double    **accinf;
static int phase;
X int   itstep;

X double fx;
static double    upsi,upsi0,upsi1,upsist,psi,psi0,
  psi1,psist,psimin,
  phi,/*phi0,*/phi1,phimin,fx0,fx1,
  fxst,o8fmin,b2n,b2n0,xnorm,x0norm,sig0,dscal,o8dnorm,d0norm;

static double    sig,sigmin,dirder,cosphi,upsim;
X double  *x;
static double *x0,*x1,*xmin,*d,*d0,
            *dd,*difx,*resmin;

X double **gres, *gradf;
static double  gfn,*qgf,*gphi0,*gphi1,*gresn;

static int   *perm,*perm1,*colno,rank;
static double    **qr,*betaq,*diag,*cscal,*colle;

/* colno also used o8qpso with double length ! */

X double **a;
X double **hess;
X int calc_hessian;

static double    /* scalm,scalm2,*/ *diag0,matsc;

static int   *violis,*alist,*o8bind,
            *o8bind0, *aalist,*clist;

X double *u, *w;
static double    *u0,
            *w1,*res,
            *res0,*res1,
            *resst,scf,scf0,
            *yu,*slack,infeas,*work;

X int   iterma;
X double    del,del0,delmin,tau0,tau;
static double del01, ny;
X double epsx;
static double    smalld,smallw,rho,rho1,eta,c1d,
  scfmax,updmy0,tauqp,taufac,taumax;

static double    alpha,beta,theta,sigsm,sigla,delta,stptrm;
static double    delta1,stmaxl;

static double    level;
static int   clow,lastdw,lastup,lastch;

X FILE      *prou,*meu;

static double    *ug,*og;

X double    *low,*up,big;

X int   nreset;

static double    *xst;
