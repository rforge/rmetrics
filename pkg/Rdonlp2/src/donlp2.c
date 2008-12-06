#include "o8gene.h"

/* Modified by Diethelm Wuertz to work with R2.6beta on */
#include <R.h>

/* Modified by Ryuichi Tamura to embed donlp2 in R */
#include <R_ext/Boolean.h>
#include <R_ext/Error.h>
#include <R_ext/Print.h>
#include <R_ext/Constants.h>
#include <R_ext/RS.h> /* Calloc() in eshessf() */
#include <Rinternals.h>

/* Conditions of use:                                                        */
/* 1. donlp2 is under the exclusive copyright of P. Spellucci                */
/*    (e-mail:spellucci@mathematik.tu-darmstadt.de)                          */
/*    "donlp2" is a reserved name                                            */
/* 2. donlp2 and its constituent parts come with no warranty, whether ex-    */
/*    pressed or implied, that it is free of errors or suitable for any      */
/*    specific purpose.                                                      */
/*    It must not be used to solve any problem, whose incorrect solution     */
/*    could result in injury to a person , institution or property.          */
/*    It is at the users own risk to use donlp2 or parts of it and the       */
/*    author disclaims all liability for such use.                           */
/* 3. donlp2 is distributed "as is". In particular, no maintenance, support  */
/*    or trouble-shooting or subsequent upgrade is implied.                  */
/* 4. The use of donlp2 must be acknowledged, in any publication which       */ 
/*    contains                                                               */
/*    results obtained with it or parts of it. Citation of the authors name  */
/*    and netlib-source is suitable.                                         */
/* 5. The free use of donlp2 and parts of it is restricted for research      */
/*    purposes                                                               */
/*    commercial uses require permission and licensing from P. Spellucci.    */

/* d o n l p  2                                                              */

/*    version 28/11/2001 (*)                                                 */
/*    tauqp dependent on scalres only                                        */
/*    weights computed in a modified version in the singular case            */
/*    some comparisons relative to zero changed                              */
/*    error-return for function evaluation as added feature                  */
/*    termination of QP-solver changed (d not set to zero)                   */
/*    new version of BFGS: if nr  =  0 take Powell's update                  */
/*    no suppression of update beforehand (with exception dg = 0)            */
/*    plus some minor corrections                                            */

/*    for consistency reasons variable names cf and cgf changed into         */
/*    icf and icgf                                                           */
/*    added feature numerical differentiation of order 1,2,6                 */
/*    requires new parameters epsfcn, taubnd, difftype                       */
/*    added feature of external blockwise evaluation of functions            */
/*    rather than individual evaluation                                      */
/*    requires new parameter bloc                                            */
/*    new feature of user-supplied scaling of x                              */

/* ************************************************************************* */
/* (*) using a c-version of donlp2 as of 1998 made by                        */
/*     by S. Schoeffert, ASB, Bourges, France                                */
/*     dynamic memory allocation added by k.s. Cover                         */
/*     copyrights and right of commercial exploitation remaining by contract */
/*     with P. Spellucci                                                     */
/* ************************************************************************  */
/*  new function format                                                      */
/*  developed in 2002/2003 by P. Spellucci copyrigth P. Spellucci            */
/*  The problem format here is                                               */
/*     min_x f(x)                                                            */ 
/*                                                                           */
/*     subject to                                                            */
/*       low(i) <= x(i)   <=   up(i) , i=1,..,n                              */
/*     low(i+n) <= (Ax)_i <= up(i+n) , i=1,nlin     nlin=0 is allowed        */
/*low(i+nlin+n) <= c_i(x) <= up(i+n+nlin)  i=1,nonlin, nonlin=0 allowe       */
/*                                                                           */
/*                                                                           */
   
/*  low(i)=up(i) is allowed .   In this case only one constraint is used     */ 
/*  low(i)=-big, up(i)=big is allowed, big is userdefined                    */
/*  these are a total of 2*(n+nlin+nonlin) constraints, numbered             */
/*  consecutively                                                            */
/*  such that odd numbers correspond to the lower and even numbered to the   */
/*  upper                                                                    */
/*  bound.  A is to be stored in the nlin first columns of gres.             */
/*  the user evaluation routines for the constraints only deal with the      */
/*  nonlinear part and the objective function.                               */
/*  gradients of the bound constraints are never explicitly stored .         */
/*  the relevant parts of the code have been changed to reflect this.        */
/*  the parameters                                                           */ 
/*   n,nlin,nonlin,big                                                       */
/*   low                                                                     */
/*   up                                                                      */
/*   A  = gres[][i],   i=1,...,nlin                                          */
/*   are to be set (or read from a file) in the basic routine user_init      */
/*   the parameters tau0, del0, delmin etc keep there meaning and usage.     */
/*                                                                           */
/*  A new form of evaluation interface is used for the gradients:            */
/*  only the gradients of the nonlinear constraints must be evaluated,       */
/*  for a given list of indizes, that is grad_c_i , i in list.               */
/*  Also the nonlinear functions c_i(x) are evaluated blockwise              */ 
/*  bug fix: mes-file written before opened (correction of initial value)    */
/*  bug fix: dead loop in o8qpdu in case of degenerate and illconditioned    */
/*      primal solution                                                      */
/*   bug fix:   log(b2n) now only for b2n>0                                  */
/*   bug fix in user_eval.c: nres replaced by nonlin                         */
/*  fix of a problem with glibc : bind and bind0 renamed to o8bind and o8bind0*/
/*****************************************************************************/





/* ARRAY_BORDER_CHECK: Check array borders for indices writing off
   the end of arrays.  Turn off by commenting out the line
   "#define ARRAY_BORDER_CHECK".  It is recommended that  ARRAY_BORDER_CHECK
   be turned off during regular use and  turned on for debugging
   and testing only.  The function "checkArrayBorders" checks if there has been
   any array border violations up to point where it is called.  It is currently
   called at the beginning and end of donlp2 if border checking is turned on.
   To find the location of the first border violation insert additional 
   calls to the function narrowing the bracketing on the initial border
   violation occurance.  Additionally, the values of the border counters
   (d_borders_count, i_borders_count, l_borders_count) can be printed
   out at various locations in global_mem_malloc to find out
   which array has been assigned which index. */

/* #define ARRAY_BORDER_CHECK  */


#ifdef ARRAY_BORDER_CHECK
#define ABC 1
#else
#define ABC 0
#endif

#ifdef ARRAY_BORDER_CHECK 

#define ABC_NUM_1DARRAYS 100000
static double  *d_borders[2*ABC_NUM_1DARRAYS];
static int *i_borders[2*ABC_NUM_1DARRAYS];
static int *l_borders[2*ABC_NUM_1DARRAYS];
static const double  d_unique = 1.23413936727;
static const int i_unique = 972897426;
static const int l_unique = 953712449;
static int d_borders_count = 0;
static int i_borders_count = 0;
static int l_borders_count = 0;
extern void checkArrayBorders(char *str); 
#endif


/* Global variables only used locally */

static double   *donlp2_yy; 
static double   *o8bfgs_dg,*o8bfgs_adx,*o8bfgs_ltdx, 
         *o8bfgs_gtdx,*o8bfgs_updx,*o8bfgs_updz;
static double   *o8opti_qtx;
static double   *o8opti_yy,*o8opti_yx,*o8opti_trvec,*o8opti_con;
static int  *o8opti_delist,*o8opti_bindba;
static double   *o8eval_con;
static double   *o8unim_step;
static double   *o8dec_qri,*o8dec_qri0;
static double   *o8elim_qri,*o8elim_y,*o8elim_rhsscal;
static int  *o8elim_col;
static double   *o8sol_xl;
static double   *o8upd_sdiag,*o8upd_rn1,*o8upd_w;
static double   *o8qpdu_g0,*o8qpdu_ci0,*o8qpdu_cii,
                    *o8qpdu_cei,
                    *o8qpdu_xd,*o8qpdu_s,*o8qpdu_z,*o8qpdu_vr;
static double   *o8qpdu_xdold,*o8qpdu_udold;
static int  *o8qpdu_ai,*o8qpdu_iai,*o8qpdu_iaexcl,*o8qpdu_aiold;
static int  *o8qpdu_eqlist,*o8qpdu_iqlist;
static double   *o8qpdu_y,*o8qpdu_mult;
static int  *o8qpdu_qpdel;
static int  *escongrad_errloc;
static double   *escongrad_fhelp1,*escongrad_fhelp2,
         *escongrad_fhelp3,*escongrad_fhelp4,
         *escongrad_fhelp5,*escongrad_fhelp6;


/* initialize global variables in every invokations of donlp() */
static void
global_vars_init(void)
{
#ifdef ARRAY_BORDER_CHECK   
  d_borders_count = 0;
  i_borders_count = 0;
  l_borders_count = 0;
#endif
  
  runtim = optite = 0.0;
  itstep = phase = 0;
  upsi = upsi0 = upsi1 = upsist = psi = psi0 = 
    psi1 = psist = psimin = 
    phi =  phi1 = phimin = fx = fx0 = fx1 = 
    fxst = o8fmin = b2n = b2n0 = xnorm = x0norm = sig0 = dscal =
    o8dnorm = d0norm = 0.0;

  sig = sigmin = dirder = cosphi = upsim = 0.0;
  matsc = 0.0;
  scf = scf0 = infeas = 0.0;

  iterma = 0;
  del=del0=del01=delmin=tau0=tau=ny=0.0;
  smalld=smallw=rho=rho1=eta=epsx=c1d=
    scfmax=updmy0=tauqp=taufac=taumax=0.0;

  alpha=beta=theta=sigsm=sigla=delta=stptrm=0.0;
  delta1=stmaxl=0.0;

  level=0.0;
  clow=lastdw=lastup=lastch=0.0;

  prou = meu = (FILE *)NULL;

  nreset = 0;

  bloc = valid = corr = 0;
  difftype = 0;

  n=nr=nres=nlin=nonlin=nstep=ndualm=mdualm=0;
  epsmac=tolmac=deldif=0.0;
  memset(name, '\0', 41);
  epsdif = 0.0;
  intakt=te0=te1=te2=te3=singul=0;
  ident=eqres=silent=fsilent=analyt=cold=0;
  icf=icgf=cfincr=0;
  ffuerr=0;

  qpterm=qpitma=0;
  iptr=iqtr=0;
  sstr=riitr=0.0;
    
  accinf = (double **)NULL;
  x = x0 = x1 = xmin = d = d0 = dd = difx = resmin =
    gradf = qgf = gphi0 = gphi1 = (double *)NULL;
  gres = (double **)NULL;
  gresn = (double *)NULL;
  perm = perm1 = colno = (int *)NULL;
  qr = (double **)NULL;
  betaq = diag = cscal = colle = (double *)NULL;

  a = hess = (double **)NULL; calc_hessian = 0;
  diag0 = (double *)NULL;
  violis = alist = o8bind = o8bind0 = aalist = clist = (int *)NULL;

  u = u0 = w = w1 = res = res0 = res1 = resst = yu = slack =
    work = ug = og = low = up = xst = (double *)NULL;

  xtr = xsc = fu = (double *)NULL;
  fugrad = fud = (double **)NULL;

  val = llow = cres = cgres = confuerr = nonlinlist = (int *)NULL;
  gunit = (int **)NULL;
  gconst = cfuerr = (int *)NULL;

  np = (double *)NULL;
  xj = (double **)NULL;
  ddual = (double *)NULL;
  r = (double **)NULL;
  ud = ud1 = (double *)NULL;
  aitr = (int *)NULL;

  /* o8gene.h */
  qpterm=qpitma=0;
  ndual=mi=me=iq;
  np=(double *)NULL;
  rnorm=rlow=0.0;
  xj=(double **)NULL;
  ddual=(double *)NULL;
  r=(double **)NULL;
  ud=(double *)NULL;
  ud1=(double *)NULL;
  iptr=iqtr=0;
  aitr=(int *)NULL;
  sstr=riitr=0.0;

  donlp2_yy = o8bfgs_dg = o8bfgs_adx = o8bfgs_ltdx = o8bfgs_gtdx =
    o8bfgs_updx = o8bfgs_updz = o8opti_qtx = o8opti_yy =
    o8opti_yx = o8opti_trvec = o8opti_con = (double *)NULL;

  o8opti_delist = o8opti_bindba = (int *)NULL;

  o8eval_con = o8unim_step = o8dec_qri = o8dec_qri0 = 
    o8elim_qri = o8elim_y = o8elim_rhsscal = (double *)NULL;

  o8elim_col = (int *)NULL;

  o8sol_xl = o8upd_sdiag = o8upd_rn1 = o8upd_w = o8qpdu_g0 =
    o8qpdu_ci0 = o8qpdu_cii = o8qpdu_cei = o8qpdu_xd =
    o8qpdu_s = o8qpdu_z = o8qpdu_vr = o8qpdu_xdold = o8qpdu_udold =
    (double *)NULL;

  o8qpdu_ai = o8qpdu_iai = o8qpdu_iaexcl = o8qpdu_aiold = 
    o8qpdu_eqlist = o8qpdu_iqlist = (int *)NULL;

  o8qpdu_y = o8qpdu_mult = (double *)NULL;

  o8qpdu_qpdel = escongrad_errloc = (int *)NULL;
  escongrad_fhelp1 = escongrad_fhelp2 = escongrad_fhelp3 =
    escongrad_fhelp4 = escongrad_fhelp5 = escongrad_fhelp6 = (double *)NULL;

  return;
}


void donlp2(void) {

    void    o8st   (void);
    void    o8fin  (void);
    void    o8opti (void);
    double  o8vecn (int nl,int nm,double x[]);
    void    esgradf(double x[],double gradf[]);
    void    eshessf(double x[],double **hess);
    void global_mem_malloc();
    /* void global_mem_free(); */ /* moved to teardown() in Rdonlp2.c */
    
    void    setup0 (void);
    void    setup  (void);
    void user_init_size (void);
    void user_init (void);
    void    o8msg(int num);
    void    o8elim(void);
    static double   term ;
    static int  i,j,k,iz;
    static char     fil[13],xxx[9] = "xxxxxxxx",name1[9];    

    global_vars_init(); /* added by RT to initialize all 'global' variables */
    
/* --------------->  step one of problem initialization by user               */
    /*  user_init_size must initialize  n, nlin, nonlin, iterma, nstep.       */
    /*  These values may not be changed.                                      */

    user_init_size();
    ndualm = 2*n+nlin+nonlin;
    mdualm = 2*(n+nlin+nonlin);

    /* allocate the global memory */

    global_mem_malloc(); 
#ifdef ARRAY_BORDER_CHECK
    checkArrayBorders("donlp2: after global_mem_malloc"); 
#endif

    /* default settings of new parameters */
    
    bloc     = FALSE;
    analyt   = TRUE;
    valid    = FALSE;
    epsfcn   = 1.e-16;
    difftype = 3;
    taubnd   = 1.;
    for (i = 1 ; i <= n ; i++) {
        xsc[i] = one;
        xtr[i] = zero;
    }
    epsdif = tm8;
    for (i = 0 ; i <= nlin+nonlin ; i++) {
        val[i]    = FALSE;
        if ( i > 0 ) gresn[i] = one;
    }
    ffuerr = FALSE;

    /* some standard initialization which may be overwritten by */
    /* user_init or setup                                       */

    silent = FALSE; fsilent=FALSE;
    
    /* the interactive input feature is no longer supported here. for the sake */
    /* of easy revision the variable is supplied here however                  */
    /* if intakt is set TRUE, output to protocol file is copied to stdout in   */
    /* addition                                                                */
    
    intakt = FALSE;
    te0    = TRUE;   /* control running optimizer on stdout */
    te1    = FALSE;   /* information about iteration in compressed form */
    te2    = FALSE;   /* detailed output in case of trouble */
    te3    = FALSE;   /* print gradients and Hessian        */
    cold   = TRUE;
    big    = 1.e20 ; 

/* ->   here user initialization continued                                    */

    
    /*  user_init must initialize analyt, epsdif, del0, tau0 ,                */
    /*  low, up, and the gradients of the linear constraints, if any          */
    /*  (stored in gres[1:n][j], j=1,nlin)                                    */
    /*  bloc                                                                  */
    /*  analyt and if analyt = FALSE then also epsfcn , taubnd , viobnd ,     */
    /*  difftype                                                              */
    /*  and the initial value for x                                           */
    /* may also change settings of all variables initialized above            */

    user_init();

/*   open files for output if wanted                                          */
    
    j = 0;
    while ( name[j] == ' ' ) {
        j = j+1;
    }
    if ( name[j] == '\0' ) {
        strcpy(name1,xxx);
    } else {
        k = j+1;
        while ( name[k] != ' ' && name[k] != '\0' && k-j < 8 ) {
            k = k+1;
        }
        strncpy(name1,&name[j],k-j);
        name1[k-j] = '\0';
        
        for (i = 0 ; i <= k-j-1 ; i++) {
            iz = name1[i];
            if ( iz < 48 || ( iz > 57 && iz < 65 )
            || ( iz > 90 && iz < 97 ) || iz > 122 ) name1[i] = 'x';
        }
        if ( k - j < 8 ) strncat(name1,xxx,8-k+j);
    }
    if ( ! silent && ! fsilent ) 
    {
        strcpy(fil,name1);
        meu  = fopen(strcat(fil,".mes"),"w");
        strcpy(fil,name1);
        prou = fopen(strcat(fil,".pro"),"w");
    }




/* ************************************************************************** */
/* ***** automatic correction of del0                                         */
/******************************************************************************/

    for ( i = 1 ; i <= n ; i++ )
    {
        if ( xsc[i] == zero ) 
        {
            REprintf("scaling variable %i is zero\n",i);
            error("Rdonlp2 Internal Error");
        }
    }
    nres = n+nlin+nonlin ; 
    for ( i = 1 ; i <= nres ; i++ )
    {
       if ( i <= n ) 
       {
         term = xsc[i] ;
       }
       else
       {
         term = one ;
       }
       if (! (low[i] == up[i]) )
       { 
          del0 = min ( del0 , (up[i]-low[i])/(four*term) );
       }
    }
    delmin = tm6;

    for (i = 1 ; i <= n ; i++) 
    {
        
        ug[i] = low[i]; 
        if ( ug[i] <= -big )    
        {
           llow[i] = FALSE ; 
        }   
        else
        {
           llow[i] = TRUE ;  
        }   
        og[i] = up[i];
        if ( og[i] >= big ) 
        {
           lup[i] = FALSE ; 
        }
        else 
        {
           lup[i] = TRUE ;  
        }
           
    }      
    
    for (i=1; i<=n; i++)
    {
       if ( x[i] < low[i] || x[i] > up[i] )
       {
          corr = TRUE ;
          if ( llow[i] && lup[i]  ) x[i] = (up[i]+low[i])/2.0;
          if ( llow[i] && !lup[i] ) x[i] = low[i]+2.0*del0;
          if ( lup[i]  && !llow[i]) x[i] = up[i]-2.0*del0; 
       }
    }   

    if ( corr && ! fsilent ) o8msg(13);

    for (i = 1 ; i <= n ; i++) {
        xst[i] = x[i];
        x[i] = x[i]/xsc[i];
        o8opti_yy[i] = xsc[i];

    }
    for (i = 1 ; i <= n ; i++) {
    
        /* ug and og have been evaluted for the original variables */
        /* here we use them internally for the scaled ones         */
        /* ug = low up to scaling etc                              */
        ug[i] = ug[i]/xsc[i];
        og[i] = og[i]/xsc[i];
    }
    for ( i = 1 ; i <= nlin ; i++ )
    { 
 /*   rescale the gradients of the linear constraints */
      gresn[i] = zero ;
      for ( j = 1 ; j <= n ; j++ )
      {
          gres[j][i] = xsc[j]*gres[j][i] ;
          gresn[i] += pow(gres[j][i],2);
      }
      gresn[i] = max ( one , sqrt(gresn[i]) ) ;
      cgres[i] = 1 ;
      val[i]   = TRUE ;
    }
/*    the sign of the gradient is determined from the current constraint */
/*    and stored in gres[0][i]. done afterwards                          */
    nreset = n;

    o8st();
    
    /* setup may change standard settings of parameters  */
    /* and add some computations in the user environment */
    qpitma = nres  ;
    setup();
    
    if ( taubnd <= 0 ) {
      REprintf("taubnd le zero is not allowed ");
      error("Rdonlp2 Internal Error");
    }
    for ( i = 1 ; i<= n ; i++ ) {
      if ( o8opti_yy[i] != xsc[i] ) {
        REprintf("setup has changed xsc, not allowed");
        error("Rdonlp2 Internal Error");
      }
    }
    /* preevaluation of gradients of linear functions    */
    /* done now in user_init                             */


    runtim = clock();
    /* check for redundant linear equality constraints   */
    o8elim();
    
    

    /* call the optimizer */
    
    o8opti();
    
    runtim = (clock()-runtim)/CLOCKS_PER_SEC;

    if (calc_hessian) eshessf(x, hess);
    /* do final solution check and output */
    o8fin();


#ifdef ARRAY_BORDER_CHECK
    checkArrayBorders("donlp2: before global_mem_free"); 
#endif

    /* free up memory : move to "teardown()" in Rdonlp2.c */
    /* global_mem_free(); */

    return;
}

/* **************************************************************************** */
/*        initialization program , standard parameter settings done here        */
/* **************************************************************************** */
void o8st(void) {

    void o8msg    (int num);
    void user_eval(double xvar[],int mode);

    static int  i,j/*,k*/;
    /* static double   tol1 ,bd0,infiny,gxi,hxi,term; */
    static double infiny;
    static time_t   tim;

#if 0    
    epsmac = pow(two,-20);
    
    L100:

    epsmac = epsmac/two;
    term   = one+epsmac;
    
    if ( term != one ) goto L100;
#endif

    epsmac = DOUBLE_EPS; /* modified by RT to use R's machine epsilon */

#if 0    
    epsmac = epsmac+epsmac;

    tolmac = epsmac;
    
    L200:

    tol1   = tolmac;
    tolmac = tolmac/twop4;
    
    if ( tolmac != zero ) goto L200;
    
    tolmac = tol1;
#endif

    tolmac = DOUBLE_XMIN; /* modified by RT to use R's machine_xmin */
    
    /* epsmac machine precision, tolmac smallest machine number */
    /* larger than zero (approximately , base 16 for exponent   */
    /* therefore division by 16 assumed)                        */

    /*                        ***** warning *****                        */
    /* on some machines the computation of tolmac may result in an error */
    /* because underflow is not accepted as zero as is assumed here      */

    /* warning: the computation of epsmac may give faulty results if     */
    /* compiler uses value residing in register instead of the one stored */
    /* back to memory. with gcc use the switch  -ffloat-store in order */
    /* to prevent this                                                 */

    if ( tau0 == zero ) tau0 = one;
    if ( del0 == zero ) del0 = tau0*p5;
    
    if ( nreset > n  ) nreset = n;
    if ( nreset <= 4 ) nreset = 4;
    
    /* standard initialization */
    
    lastch = 0;
    lastdw = 0;
    lastup = 0;
    level  = one;
    tau    = tm1;
    epsx   = tm5;
    sigsm  = sqrt(epsmac);
    smalld = tm1;
    
    /* formerly tm2. smalld has much influence on the Maratos-effect */
    
    smallw = exp(two*log(epsmac)/three);
    rho    = tm6;
    rho1   = tm10;
    del01  = del0/tp1;
    delmin = min(del01,max(tm6*del0,smallw));
    if ( ! analyt ) delmin = min(del01,max(epsdif,delmin));

    c1d    = tm2;
    scfmax = tp4;
    taufac = tp1;
    taumax = pow(scfmax,2);
    updmy0 = tm1;
    
    /* take del0 and tau0 from  user_init in the user function suite       */
    /* may be modified by subsequent call of setup                         */

    
    infiny = epsmac/tolmac;
    fx     = zero;
    b2n    = zero;
    b2n0   = zero;
    nres   = n+nlin+nonlin;
/*  but remember there are formally 2*nres constraints */
    if ( cold ) 
    {
    /*  initialize the quasi Newton update                 */
      for (i = 1 ; i <= n ; i++) 
      {
         for (j = 1 ; j <= n ; j++) 
         {
            a[i][j] = zero;
         }
         a[i][i]  = one;
         diag0[i] = one;
      }
      matsc = one ;
    }
      for (i = 1 ; i <= n ; i++) 
      {
        diag[i] = zero;
      }
      for ( i = 1 ; i <= nres ; i++ )
      {
        for (j = 1 ; j <= n ; j++) 
        {
          qr[j][i]   = zero;
        }
      }
      for ( i = nlin+1 ; i <= nlin+nonlin ; i++ )
      {
         for ( j = 1 ; j <= n ; j++ )
         {
           gres[j][i] = zero;
         }
         gres[0][i] = one ;
      }
    
    
    /* nonlinear gradients not yet evaluated */
    for ( i = nlin+1 ; i<=nlin+nonlin ; i++ ) val[i] = FALSE ;    
  
    if ( bloc )  
    {    
       valid = FALSE ;
       /* user_eval must reset valid */
       user_eval(x,1);
    }
    /* this call gives all function and gradient values in fu[] and fugrad[][]*/
    /* later call of escon and escongrad simply gives a move in this case     */


    for ( i = 1 ; i <= 2*nres ; i ++) 
    {
        o8bind[i]  = 0;
        o8bind0[i] = 0;
        u[i]     = zero;
        u0[i]    = zero;
        /* initial weights of the penalty-term */
        if ( cold ) w[i] = one;
    }   

    for ( i = 1 ; i <= nlin+nonlin ; i++ )
    {
        cres[i] = 0 ;
        cgres[i] = 0 ;
    }

    clow = one;
    ny   = two;

    /* scf  = weight factor for objective function    */
    /* scf0 = damping factor for tangential direction */
    
    scf    = one;
    scf0   = one;
    sigla  = twop11;
    beta   = four;  /* formerly two */
    alpha  = tm1;
    delta1 = p9;
    delta  = tm3;   /* delta = tm2  formerly */
    theta  = p9;    /* theta = 0.99 formerly */
    icf    = 0;
    icgf   = 0;
    if ( !silent && ! fsilent ) {
        fprintf(prou,"donlp2-intv 21/06/04, copyright P. Spellucci\n");
        
        time(&tim);
        
        fprintf(prou,"%s",ctime(&tim));
        fprintf(prou,"%s\n", name);
        
        fprintf(meu, "donlp2-intv 21/06/04, copyright P. Spellucci\n");
        fprintf(meu, "%s",ctime(&tim));
        fprintf(meu, "%s\n", name);
    }
    return;
}

/* **************************************************************************** */
/*                        do final solution check and output                    */
/* **************************************************************************** */
void o8fin(void) {

    void    solchk(void);

    static int  i,j,k,ih1,ih2,ih3,ih5,ih6,ih7,ih8,ih9;
    static double   umin,term;
    static int  nsing,crtot,cgrtot,nupreg,nbfgs1,nresta;
    static char     line[65];

    /* termination reason + 11 = message number */
    
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

    if ( scf != zero ) {
        for (i = 1 ; i <= 2*nres ; i++) {
            u[i] = u[i]/scf;
        }
    }
    /* in solchk the user might add some additional checks and/or output */

    solchk();

    if ( silent && ! intakt ) return;

    if ( intakt && ! silent )Rprintf("%s\n", name);
    
    if ( ! silent ) {
        if ( intakt ) {
           Rprintf(  "\n     n= %9i    nlin= %9i    nonlin= %9i\n", n,nlin,nonlin);
           Rprintf(  "\n  epsx= %9.3e sigsm= %9.3e\n"       , epsx,sigsm);
           Rprintf(  "\nstartvalue\n");
            for (i = 1 ; i <= n ; i++) {
               Rprintf(  " %14.7e ", xst[i]);
                if ( i % 5 == 0 || i == n )Rprintf(  "\n");
            }
        }
    if (! fsilent){
      fprintf(prou,"\n     n= %9i    nlin= %9i    nonlin= %9i\n", 
          n,nlin,nonlin);
      fprintf(prou,"\n  epsx= %9.3e sigsm= %9.3e\n"       , epsx,sigsm);
      fprintf(prou,"\nstartvalue\n");
      for (i = 1 ; i <= n ; i++) {
            fprintf(prou," %14.7e ", xst[i]);
            if ( i % 5 == 0 || i == n ) fprintf(prou,"\n");
      }
    }
    }
    if ( intakt && ! silent ) {
       Rprintf("\n  eps= %9.2e  tol= %9.2e del0= %9.2e delm= %9.2e tau0= %9.2e\n",
        epsmac,tolmac,del0,delmin,tau0);
       Rprintf(  "  tau= %9.2e   sd= %9.2e   sw= %9.2e  rho= %9.2e rho1= %9.2e\n",
        tau,smalld,smallw,rho,rho1);
    }
    if ( ! silent && ! fsilent ) { 
        fprintf(prou,
               "\n  eps= %9.2e  tol= %9.2e del0= %9.2e delm= %9.2e tau0= %9.2e\n",
        epsmac,tolmac,del0,delmin,tau0);
        fprintf(prou,
                 "  tau= %9.2e   sd= %9.2e   sw= %9.2e  rho= %9.2e rho1= %9.2e\n",
        tau,smalld,smallw,rho,rho1);
    }
    if ( ! silent ) {
      if (! fsilent) fprintf(prou,
        " scfm= %9.2e  c1d= %9.2e epdi= %9.2e\n  nre= %9i anal= %9i\n",
        scfmax,c1d,epsdif,nreset,analyt);
        if ( intakt )Rprintf(
        " scfm= %9.2e  c1d= %9.2e epdi= %9.2e\n  nre= %9i anal= %9i\n",
        scfmax,c1d,epsdif,nreset,analyt);
    }
    if ( ! silent && ! analyt ) {
      if (! fsilent )fprintf(prou," vbnd= %9.2e efcn= %9.2e diff=%1i\n"
        , taubnd,epsfcn,difftype);
        if ( intakt )Rprintf("taubnd= %9.2e epsfcn= %9.2e difftype=%1i\n"
        , taubnd,epsfcn,difftype);
    }
    i    = 0;
    j    = 0;
    umin = zero;
    for (k = 1 ; k <= nlin+nonlin ; k++) {
        i = i+ cres[k];
        j = j+cgres[k];
    }
    for ( k = 1 ; k <= nres ; k++ ) {
        if ( low[k] != up[k]  ) umin =min( min( umin,u[2*k-1] ) ,u[2*k]);
    }
    crtot  = i;
    cgrtot = j;
    nsing  = 0;
    nresta = 0;
    nupreg = 0;
    nbfgs1 = 0;
    for (k = 1 ; k <= itstep ; k++) {
        if ( accinf[k][10] == one ) nsing  = nsing+1;
        if ( accinf[k][27] == one ) nbfgs1 = nbfgs1+1;
        
        /* for the Pantoja Mayne update */
        
        if ( accinf[k][29] == zero && accinf[k][27] == one ) nupreg = nupreg+1;
        if ( accinf[k][27] == -one ) nresta = nresta+1;
    }
    k = (int)optite+11;
    if ( k >= 1 && k <= 19 ) {
        strcpy(line,messag[k]);
    } else {
        strcpy(line,"variable optite undefined on exit");
    }
    if ( intakt && ! silent ) {
       Rprintf(      "\n termination reason:\n %s\n",line);
       Rprintf(        " evaluations of f                    %9i\n",    icf);
       Rprintf(        " evaluations of grad f               %9i\n",    icgf);
       Rprintf(        " evaluations of constraints          %9i\n",    crtot);
       Rprintf(        " evaluations of grads of constraints %9i\n",    cgrtot);
       Rprintf(        " final scaling of objective          %13.6e\n", scf);
       Rprintf(        " norm of grad(f)                     %13.6e\n", gfn);
       Rprintf(        " lagrangian violation                %13.6e\n", b2n);
       Rprintf(        " feasibility violation               %13.6e\n", upsi);
       Rprintf(        " dual feasibility violation          %13.6e\n", umin);
       Rprintf(        " optimizer runtime sec's             %13.6e\n", runtim);
    }
    if ( ! fsilent && ! silent ) {
        fprintf(prou,"\n termination reason:\n %s\n",line);
        fprintf(prou,  " evaluations of f                    %9i\n",    icf);
        fprintf(prou,  " evaluations of grad f               %9i\n",    icgf);
        fprintf(prou,  " evaluations of constraints          %9i\n",    crtot);
        fprintf(prou,  " evaluations of grads of constraints %9i\n",    cgrtot);
        fprintf(prou,  " final scaling of objective          %13.6e\n", scf);
        fprintf(prou,  " norm of grad(f)                     %13.6e\n", gfn);
        fprintf(prou,  " lagrangian violation                %13.6e\n", b2n);
        fprintf(prou,  " feasibility violation               %13.6e\n", upsi);
        fprintf(prou,  " dual feasibility violation          %13.6e\n", umin);
        fprintf(prou,  " optimizer runtime sec's             %13.6e\n", runtim);
    }                                         
    if ( intakt && ! silent )Rprintf("\n\n optimal value of f =  %21.14e\n", fx);
    if ( ! silent && ! fsilent ) fprintf(prou,    "\n\n optimal value of f =  %21.14e\n", fx);
    
    if ( intakt && ! silent ) {
       Rprintf(      "\n optimal solution  x =\n");
        for (i = 1 ; i <= n ; i++) {
           Rprintf(      " %21.14e", x[i]);
            if ( i % 3 == 0 || i == n )Rprintf(     "\n");
        }
    }
    if ( ! fsilent && ! silent ) {
        fprintf(prou,"\n optimal solution  x =\n");
        for (i = 1 ; i <= n ; i++) {
            fprintf(prou," %21.14e", x[i]);
            if ( i % 3 == 0 || i == n ) fprintf(prou,"\n");
        }
    }
    if ( nres != 0 && ! silent ) 
    {
      if (! fsilent ){
        fprintf(prou,"\n  multipliers are relativ to scf=1\n");
        fprintf(prou,"  nr.    constraint   multiplier        normgrad (or 1) \n");
        for (i = 1 ; i <= 2*nres ; i++) 
        {
          if ( (i+1)%2 == 0 && i >2*n )
          {
            fprintf(prou," %4i  %14.7e   %14.7e   %14.7e\n"
            , i,res[i],u[i],gresn[(i+1)/2-n]);
          }
          else
          {
            fprintf(prou," %4i  %14.7e   %14.7e \n"
            , i,res[i],u[i]);
          }
        }
      }
      if ( intakt ) 
        {
           Rprintf(  "\n  multipliers are relativ to scf=1\n");
           Rprintf(    "  nr.    constraint     multiplier norm(grad) or 1 \n");
            for (i = 1 ; i <= 2*nres ; i++)
            {
              if ( (i+1)%2 == 0 && i > 2*n  )
              {
               Rprintf(" %4i  %14.7e   %14.7e   %14.7e\n"
               , i,res[i],u[i],gresn[(i+1)/2-n]);/*added '-n' by RT */
              }   
              else
              {
               Rprintf(" %4i  %14.7e   %14.7e \n"
                  , i,res[i],u[i]);
              }
            }
        }
    }
    if ( nres != 0 && ! silent ) {
      if (!fsilent){
        fprintf(prou,"\n evaluations of restrictions and their gradients\n");
        for (i = 1 ; i <= nlin+nonlin ; i++) {
            fprintf(prou," (%6i,%6i)", cres[i],cgres[i]);
            if ( i % 5 == 0 || i == nres ) fprintf(prou,"\n");
        }
      }
      if ( intakt ) {
           Rprintf(  "\n evaluations of restrictions and their gradients\n");
            for (i = 1 ; i <= nlin+nonlin ; i++) {
               Rprintf(  " (%6i,%6i)", cres[i],cgres[i]);
                if ( i % 5 == 0 || i == nres )Rprintf(  "\n");
            }
      }
    }
    if ( itstep > 1 && optite == 0 ) itstep = itstep-1;
    if ( ! silent && ! fsilent)
      fprintf(prou,    "\n last estimate of condition of active gradients %10.3e\n",
    accinf[itstep][13]);
                    
    term = accinf[itstep][14];
    i    = itstep;
    while ( i > 1 && term == zero ) {
        i    = i-1;
        term = accinf[i][14];
    }
    if ( ! silent ) {
      if (! fsilent ){
        fprintf(prou,"\n last estimate of condition of approx. hessian  %10.3e\n",
        term);
        fprintf(prou,"iterative steps total           %5i\n", itstep);
        fprintf(prou,"# of restarts                   %5i\n", nresta);
        fprintf(prou,"# of full regular updates       %5i\n", nupreg);
        fprintf(prou,"# of updates                    %5i\n", nbfgs1);
        fprintf(prou,"# of full regularized SQP-steps %5i\n", nsing);
      }
      if ( intakt ) {
           Rprintf(  "\n last estimate of cond.nr. of active gradients  %10.3e\n",
            accinf[itstep][13]);
           Rprintf(  "\n last estimate of cond.nr. of approx.  hessian  %10.3e\n",
            accinf[itstep][14]);
           Rprintf(  "iterative steps total           %5i\n", itstep);
           Rprintf(  "# of restarts                   %5i\n", nresta);
           Rprintf(  "# of full regular updates       %5i\n", nupreg);
           Rprintf(  "# of updates                    %5i\n", nbfgs1);
           Rprintf(  "# of regularized full SQP-steps %5i\n", nsing);
      }
    }
    if ( optite < zero ) te1 = TRUE;
    if ( silent )        te1 = FALSE;
    if ( te1 && !fsilent ) {
        for (i = 1 ; i <= itstep ; i++) {
            ih1 = accinf[i][1];
            ih2 = accinf[i][9];
            ih3 = accinf[i][10];
            ih5 = accinf[i][18];
            ih6 = accinf[i][19];
            ih7 = accinf[i][22];
            ih8 = accinf[i][26];
            ih9 = accinf[i][27];
            
            fprintf(prou, 
             "%4i  fx= %13.6e scf= %13.6e psi= %13.6e ups= %13.6e\n",
            ih1,accinf[i][2],accinf[i][3],accinf[i][4],accinf[i][5]);
            fprintf(prou,
            "     del= %13.6e b20= %13.6e b2n= %13.6e  nr=%5i\n",
            accinf[i][6],accinf[i][7],accinf[i][8],ih2);
            fprintf(prou,
            "      si=%4i            u-= %13.6e c-r= %13.6e c-d= %13.6e\n",
            ih3,accinf[i][11],accinf[i][13],accinf[i][14]);
            fprintf(prou,
            "      xn= %13.6e  dn= %13.6e pha=%4i            cl=%14i\n",
            accinf[i][16],accinf[i][17],ih5,ih6);
            fprintf(prou,
            "     skm= %13.6e sig= %13.6e cf+=%5i          dir= %13.6e\n",
            accinf[i][20],accinf[i][21],ih7,accinf[i][23]);
            fprintf(prou,
            "     dsc= %13.6e cos= %13.6e vio=%5i\n",
            accinf[i][24],accinf[i][25],ih8);
            fprintf(prou,
            "     upd=%5i           tk= %13.6e xsi= %13.6e\n",
            ih9,accinf[i][28],accinf[i][29]);
            
            if ( accinf[i][10] == 1. ) {
                fprintf(prou,"     qpt= %13.0e tqp= %13.6e sla= %13.6e\n",
                accinf[i][30],accinf[i][31],accinf[i][32]);
            }
        }
    }
    /*  accinf a c c u m u l a t e d   i n f o r m a t i o n                    */
    /*  on iteration sequence                                                   */
    /*  1: step-nr                                                              */
    /*  2: f(x-k) current value of objective (zero in feasibility improvement   */
    /*            phase (-1) )                                                  */
    /*  3: scf    internal scaling of objective (zero in phase -1)              */
    /*  4: psi    the weighted penalty-term                                     */
    /*  5: upsi   the unweighted penalty-term (L1-norm of constraint vector)    */
    /*  6: del_k_1 bound for currently active constraints                       */
    /*  7: b2n0   L2-norm of projected gradient, based on constraints in level  */
    /*            delmin and below, measured in the norm induced by the         */
    /*            inverse hessian estimate                                      */
    /*  8: b2n    L2-norm of projected gradient based on del_k_1                */
    /*  9: nr     number of binding constraints                                 */
    /* 10: sing   if 1, the binding constraints don't satisfy the regularity    */
    /*            condition                                                     */
    /* 11: umin   infinity norm of negative part of multiplier                  */
    /* 12: -------------                                                        */
    /* 13: cond_r condition number of diagonal part of QR-decomp. of normalized */
    /*            gradients of binding constraints                              */
    /* 14: cond_h condition number of diagonal of Cholesky-factor of updated    */
    /*            full hessian                                                  */
    /* 15: scf0   the relative damping of tangential component if upsi>tau0/2   */
    /* 16: xnorm  L2-norm of x                                                  */
    /* 17: dnorm  L2-norm of d (correction from QP -subproblem, unscaled)       */
    /* 18: phase  -1 : infeasibility improvement phase, 0: initial optimization */
    /*            1  : binding constraints unchanged , 2: d small, Maratos      */
    /*                 correction in use                                        */
    /* 19: c_k    number of decreases of penalty weights                        */
    /* 20: wmax   infinity norm of weights                                      */
    /* 21: sig_k  stepsize from unidimensional minimization (backtracking)      */
    /* 22: cfincr number of objective evaluations for stepsize-algorithm        */
    /* 23: dirder directional derivative of penalty-function along d (scaled)   */
    /* 24: dscal  scaling factor for d                                          */
    /* 25: cosphi cos of arc between d and d_previous. if larger theta , sig    */
    /*            larger than one (up to sigla) is tried                        */
    /* 26: violis[0] number of constraints not binding at x but hit during      */
    /*            line search                                                   */
    /* 27:        type of update for h: 1 normal P&M-BFGS-update,               */
    /*            0 update suppressed, -1 restart with scaled unit matrix ,     */
    /*            2 standard BFGS, 3 BFGS modified by Powells device            */
    /* 28: ny_k/tk modification factor for damping the projector in BFGS/       */
    /*            Pantoja-Mayne-term respectively                               */
    /* 29: 1-my_k/xsik modification factor for damping the quasi-Newton-        */
    /*            relation in BFGS for unmodified BFGS ny_k should be larger    */
    /*            than updmy0 (near one) and 1-my_k equal one./Pantoja-Mayne    */
    /*            term respectively                                             */
    /* 30: qpterm 0, if sing = -1, termination indicator of QP-solver otherwise */
    /*            1: successful, -1: tau becomes larger than tauqp without      */
    /*            slack-variables becoming sufficiently small                   */
    /*            -3: working set of QP-solver becomes linearly dependent       */
    /*            -2: infeasible QP-problem (theoretically impossible)          */
    /* 31: tauqp  weight  of slack-variables in QP-solver                       */
    /* 32: infeas L1-norm of slack-variables in QP-solver                       */

    /* moved to teardown() in Rdonlp2.c */
#if 0
    if ( ! silent ) fclose(prou);
    if ( ! silent ) fclose(meu);
#endif
    
    return;
}
/* **************************************************************************** */
/*                      prints informations if te2 = TRUE                       */
/* **************************************************************************** */
void o8info(int icase) {

    void o8mdru (double **a,int ma,int na,char head[],
                 FILE *lognum,int fix);
    void o8mdru_(double **a,   int ma,int na,char head[],
                 FILE *lognum,int fix);
            
    static int  i,j,l,k;
    static double   y,phih;
    static char     head[41];

    if(!te2) return;
    
    switch (icase) {
        
    case 1:
      if ( ! fsilent) {
        fprintf(prou,"\n\n\n");
        for (i = 1 ; i <= 80 ; i++) fprintf(prou,"=");
        fprintf(prou,"\n          %4i-th iteration step\n", itstep);
        fprintf(prou,"   scf= %11.4e psist= %11.4e   psi= %11.4e  upsi= %11.4e\n", 
        scf,psist,psi,upsi);
        fprintf(prou,"  fxst= %11.4e    fx= %11.4e\n", fxst,fx);
        fprintf(prou,"  x=\n");
        for (i = 1 ; i <= n ; i++) {
            fprintf(prou,"  %11.4e", x[i]);
            if ( i % 6 == 0 || i == n ) fprintf(prou,"\n");
        }
        fprintf(prou," valid permutation of x\n\n");
        
        for (i = 1 ; i <= n ; i++) {
            fprintf(prou,"%3i ", perm[i]);
            if ( i % 20 == 0 || i == n ) fprintf(prou,"\n");
        }
      }
        if ( phase >= 0 && te3 ) {
            strcpy(head,"quasi-Newton-matrix full update");
            
            o8mdru_(a,n,n,head,prou,FALSE);
        }
        if ( intakt ) {
           Rprintf(  "\n\n\n");
            for (i = 1 ; i <= 80 ; i++)Rprintf(  "=");
           Rprintf(  "\n          %4i-th iteration step\n", itstep);
           Rprintf(  "   scf= %11.4e psist= %11.4e   psi= %11.4e  upsi= %11.4e\n", 
            scf,psist,psi,upsi);
           Rprintf(  "  fxst= %11.4e    fx= %11.4e\n", fxst,fx);
           Rprintf(  "  x=\n");
            for (i = 1 ; i <= n ; i++) {
               Rprintf("  %11.4e", x[i]);
                if ( i % 6 == 0 || i == n )Rprintf(  "\n");
            }
           Rprintf(  " valid permutation of x\n\n");
        
            for (i = 1 ; i <= n ; i++) {
               Rprintf(  "%3i ", perm[i]);
                if ( i % 20 == 0 || i == n )Rprintf(  "\n");
            }
        }
        return;
        
    case 2:
      if (! fsilent ){
    fprintf(prou,"\n\n  del= %12.5e  b2n0= %12.5e   b2n= %12.5e   gfn= %12.5e\n", del,b2n0,b2n,gfn);
  
        if ( aalist[0] != 0 ) 
        {
            fprintf(prou,"\n\n values of restrictions\n ");
            for (i = 1 ; i <= aalist[0] ; i++) 
            {
                j=aalist[i];
                if ( j > 2*n )
                {   
                  fprintf(prou,"(%4i   %11.4e   %11.4e)  ", 
                    j,res[j],gresn[(j+1)/2-n]);
                }
                else
                {  fprintf(prou,"(%4i   %11.4e  )  ",
                    j,res[j]);
                }
                if ( i % 2 == 0 || i == aalist[0] ) fprintf(prou,"\n ");
            }
        }
        if ( aalist[0] != 0 && ! singul ) {
            fprintf(prou,"\n\n   diag[r]=\n");
            for (i = 1 ; i <= aalist[0] ; i++) {
                fprintf(prou,"  %11.4e", diag[i]);
                if ( i % 6 == 0 || i == aalist[0] ) fprintf(prou,"\n");
            }
        }
        if ( alist[0] != 0 && te3 ) {
            for (i = 1 ; i <= aalist[0] ; i++) {
              l = aalist[i];
              if ( l > 2*n ) 
              {
                fprintf(prou,"\n\n gradient of restriction nr.%4i\n ", l);
                for (j = 0 ; j <= n ; j++) {
                    fprintf(prou," %11.4e  ", gres[j][(l+1)/2-n]);
                    if ( j % 5 == 0 || j == n ) fprintf(prou,"\n ");
                }
              }
            }
        }
      }
      if ( intakt ) {
           Rprintf("\n\n  del= %12.5e  b2n0= %12.5e   b2n= %12.5e   gfn= %12.5e\n", 
            del,b2n0,b2n,gfn);
        
            if ( aalist[0] != 0 ) {
               Rprintf(  "\n\n values of restrictions\n ");
                for (i = 1 ; i <= aalist[0] ; i++) 
                {
                  j=aalist[i];
          if (!fsilent){
            if ( j > 2*n )
              {
            fprintf(prou,"(%4i   %11.4e   %11.4e)  ",
                j,res[j],gresn[(j+1)/2-n]);
              }   
            else
              {  fprintf(prou,"(%4i   %11.4e  )  ",
                 j,res[j]);
              }
          }
                  if ( i % 2 == 0 || i == aalist[0] )Rprintf(  "\n ");
                }
            }
            if ( aalist[0] != 0 && ! singul ) {
               Rprintf(  "\n\n   diag[r]=\n");
                for (i = 1 ; i <= aalist[0] ; i++) {
                   Rprintf(  "  %11.4e", diag[i]);
                    if ( i % 6 == 0 || i == aalist[0] )Rprintf(  "\n");
                }
            }
            if ( alist[0] != 0 && te3 ) {
                for (i = 1 ; i <= aalist[0] ; i++) {
                  l = aalist[i];
                  if ( l > 2*n )
                  {
                   Rprintf(  "\n\n gradient of restriction nr.%4i\n ", l);
                    /* component zero is sign */
                    for (j = 0 ; j <= n ; j++) {
                       Rprintf(  " %11.4e  ", gres[j][(l+1)/2-n]);
                        if ( j % 5 == 0 || j == n )Rprintf(  "\n ");
                    }
                  }
                }
            }
        }
        return;
        
    case 3:
        if( ! (nr == 0 || phase == -1) && !fsilent ) {
            fprintf(prou,"\n  multipliers: first estimate\n  u =\n");
            for (k = 1 ; k <= nr ; k++) {
                fprintf(prou," %4i  %11.4e", aalist[k],u[aalist[k]]);
                if ( k % 4 == 0 || k == nr ) fprintf(prou,"\n");
            }
        }
        if( ! (nr == 0 || phase == -1) && intakt ) {
           Rprintf(      "\n  multipliers: first estimate\n  u =\n");
            for (k = 1 ; k <= nr ; k++) {
               Rprintf(      " %4i  %11.4e", aalist[k],u[aalist[k]]);
                if ( k % 4 == 0 || k == nr )Rprintf(      "\n");
            }
        }
        return;
        
    case 4:
        if( ! (nr == 0 || phase == -1) && ! fsilent) {
            fprintf(prou,"\n  multipliers: second estimate\n  u =\n");
            for (k = 1 ; k <= nr ; k++) {
                fprintf(prou," %4i  %11.4e", aalist[k],u[aalist[k]]);
                if ( k % 4 == 0 || k == nr ) fprintf(prou,"\n");
            }
        }
        if( ! (nr == 0 || phase == -1) && intakt ) {
           Rprintf(      "\n  multipliers: second estimate\n  u =\n");
            for (k = 1 ; k <= nr ; k++) {
               Rprintf(      " %4i  %11.4e", aalist[k],u[aalist[k]]);
                if ( k % 4 == 0 || k == nr )Rprintf(      "\n");
            }
        }
        return;
        
    case 5:
        if( intakt )
      Rprintf(          "  condition number of r     %.15e\n",
                accinf[itstep][13]);
    if ( ! fsilent )
      fprintf(prou,    "  condition number of r     %.15e\n",
          accinf[itstep][13]);
        if ( phase == -1 ) {
      return;
        }
    else {
      if (! fsilent )
            fprintf(prou,"  condition number of a     %.15e\n",
            accinf[itstep][14]);
      if ( intakt ) {
        Rprintf(  "  condition number of a     %.15e\n",
              accinf[itstep][14]);
      }
      return;
        }
    case 6:
    
        return;
        
    case 7:
      if ( !fsilent ){
        fprintf(prou,"\n\n  phase=%3i  scf0= %11.4e\n", phase,scf0);
        fprintf(prou,    "  d =\n");
        for (i = 1 ; i <= n ; i++) {
            fprintf(prou,"  %11.4e", d[i]);
            if ( i % 6 == 0 || i == n ) fprintf(prou,"\n");
        }
        if ( phase == 2 ) {
            fprintf(prou,"\n\n  dd=\n");
            for (i = 1 ; i <= n ; i++) {
                fprintf(prou,"  %11.4e", dd[i]);
                if ( i % 6 == 0 || i == n ) fprintf(prou,"\n");
            }
        }
      }
      if ( intakt ) {
           Rprintf(  "\n\n  phase=%3i  scf0= %11.4e\n", phase,scf0);
           Rprintf(      "  d =\n");
            for (i = 1 ; i <= n ; i++) {
               Rprintf(  "  %11.4e", d[i]);
                if ( i % 6 == 0 || i == n )Rprintf(  "\n");
            }
            if ( phase == 2 ) {
               Rprintf(  "\n\n  dd=\n");
                for (i = 1 ; i <= n ; i++) {
                   Rprintf(  "  %11.4e", dd[i]);
                    if ( i % 6 == 0 || i == n )Rprintf(  "\n");
                }
            }
      }
      return;
        
    case 8:
        y    = tau0*p5;
        phih = fx*scf+psi;
    
        if ( intakt ) {
           Rprintf(  "\n\n start unimin\n\n");
           Rprintf(  "    phi= %11.4e   dphi= %11.4e    psi= %11.4e tau0/2= %11.4e\n",
            phih,dirder,psi,y);
           Rprintf(  "     fx= %11.4e  dscal= %11.4e    scf= %11.4e   upsi= %11.4e\n", 
            fx,dscal,scf,upsi);
        }
    if ( !fsilent ){
      fprintf(prou,"\n\n start unimin\n\n");
      fprintf(prou,"    phi= %11.4e   dphi= %11.4e    psi= %11.4e tau0/2= %11.4e\n",
          phih,dirder,psi,y);
      fprintf(prou,"     fx= %11.4e  dscal= %11.4e    scf= %11.4e   upsi= %11.4e\n",
          fx,dscal,scf,upsi);
        }
        return;
        
    case 9:
      if ( ! fsilent )
    fprintf(prou,"    sig= %11.4e     fx= %11.4e    psi= %11.4e   upsi= %11.4e\n",
        sig,fx1,psi1,upsi1);
      if ( intakt ) 
    Rprintf(      "    sig= %11.4e     fx= %11.4e    psi= %11.4e   upsi= %11.4e\n", 
              sig,fx1,psi1,upsi1);
        
        return;
        
    case 10:
      if (! fsilent ){
        fprintf(prou,"\n\n end unimin\n");
        fprintf(prou,  "\n sig= %11.4e  num. f-evaluations%2i\n", sig,cfincr);
        fprintf(prou,    " list of inactive hit constraints\n");
        for (i = 1 ; i <= violis[0] ; i++) {
            fprintf(prou,"%4i  ", violis[i]);
            if ( i % 13 == 0 || i == violis[0] ) fprintf(prou,"\n");
        }
        if ( violis[0] == 0 ) fprintf(prou,"none\n");
      }
      if ( intakt ) {
           Rprintf(  "\n\n end unimin\n");
           Rprintf(    "\n sig= %11.4e  num. f-evaluations%2i\n", sig,cfincr);
           Rprintf(      " list of inactive hit constraints\n");
            for (i = 1 ; i <= violis[0] ; i++) {
               Rprintf(  "%4i  ", violis[i]);
                if ( i % 13 == 0 || i == violis[0] )Rprintf(  "\n");
            }
            if ( violis[0] == 0 )Rprintf("none\n");
      }
      return;
        
    case 11:
        if ( intakt )Rprintf("additional increase of eta due to large clow\n");
        if ( !fsilent)fprintf(prou,"additional increase of eta due to large clow\n");
        
        return;
        
    case 12:
      if ( !fsilent ){
        fprintf(prou,
        "\n\n  current scaling,  scf =  %11.4e clow = %12i eta =  %11.4e\n", 
        scf,clow,eta);
        
        if(nres != 0) {
            fprintf(prou,"\n\n  scalres=\n");
            for (i = 1 ; i <= 2*nres ; i++) {
                fprintf(prou,"  %11.4e", w[i]);     
                if ( i % 6 == 0 || i == 2*nres ) fprintf(prou,"\n");
            }
        }
      }
      if ( intakt ) {
           Rprintf(
            "\n\n  current scaling,  scf =  %11.4e clow = %12i eta =  %11.4e\n", 
            scf,clow,eta);
            
            if ( nres != 0 ) {
               Rprintf(  "\n\n  scalres=\n");
                for (i = 1 ; i <= 2*nres ; i++) {
                   Rprintf(  "  %11.4e", w[i]);
                    if ( i % 6 == 0 || i == 2*nres )Rprintf(  "\n");
                }
            }
      }
      return;
        
    case 13:
        if ( accinf[itstep][27] == zero ) {
            if ( intakt )Rprintf("update suppressed\n");
            if (!fsilent)fprintf(prou,        "update suppressed\n");
        } else if ( accinf[itstep][27] == -one ) {
        if (!fsilent)fprintf(prou,"restart with scaled unit matrix\n");
            if ( intakt )Rprintf("restart with scaled unit matrix\n");
        } else {
      if (!fsilent){
            fprintf(prou,"BFGS-update\n");
            fprintf(prou," type = %14.6e\n", accinf[itstep][27]);
            fprintf(prou,"  ny  = %14.6e\n", accinf[itstep][28]);
            fprintf(prou," thet = %14.6e\n", accinf[itstep][29]);
      }
      if ( intakt ) {
               Rprintf(  "BFGS-update\n");
               Rprintf(  " type = %14.6e\n", accinf[itstep][27]);
               Rprintf(  "  ny  = %14.6e\n", accinf[itstep][28]);
               Rprintf(  " thet = %14.6e\n", accinf[itstep][29]);
      }
        }
        return;
        
    case 14:
        if ( accinf[itstep][27] == zero ) {
            if ( intakt )Rprintf("update suppressed\n");
            if (!fsilent)fprintf(prou,        "update suppressed\n");
        } else if ( accinf[itstep][27] == one ) {
      if (!fsilent){
            fprintf(prou,"BFGS-update as in Pantoja and Mayne\n");
            fprintf(prou,"  tk  = %14.6e\n", accinf[itstep][28]);
            fprintf(prou," xsik = %14.6e\n", accinf[itstep][29]);
      }
      if ( intakt ) {
               Rprintf(  "BFGS-update\n");
               Rprintf(  "  tk  = %14.6e\n", accinf[itstep][28]);
               Rprintf(  " xsik = %14.6e\n", accinf[itstep][29]);
      }
        } else {
            if ( intakt )Rprintf("restart with scaled unit matrix\n");
            if (!fsilent)fprintf(prou,        "restart with scaled unit matrix\n");
        }
        return;
        
    case 15:
        if ( intakt )Rprintf("\n\n\n singular case : full regularized SQP\n");
        if (!fsilent)fprintf(prou,        "\n\n\n singular case : full regularized SQP\n");
        if ( intakt )Rprintf("  del = %.15e\n", del);
        if (!fsilent)fprintf(prou,        "  del = %.15e\n", del);
        
        if ( intakt ) { 
           Rprintf(  "\n\n  scalres=\n");
            for (i = 1 ; i <= 2*nres ; i++) {
               Rprintf(  "  %11.4e", w[i]);
                if ( i % 6 == 0 || i == 2*nres )Rprintf(  "\n");
            }
        }
    if (!fsilent){
      fprintf(prou,"\n\n  scalres=\n");
      for (i = 1 ; i <= 2*nres ; i++) {
            fprintf(prou,"  %11.4e",w[i]);
            if ( i % 6 == 0 || i == 2*nres ) fprintf(prou,"\n");
      }
      if ( te3 && alist[0] != 0 ) 
        {
          fprintf(prou,"gradients of general constraints");
        
          for (i = 1 ; i <= aalist[0] ; i++) 
        {
          l = aalist[i];
          if ( l > 2*n )
            {
              fprintf(prou,"\n\n gradient of restriction nr.%4i\n ", l);
              for (j = 0 ; j <= n ; j++) 
            {
              fprintf(prou," %11.4e  ", gres[j][(l+1)/2-n]);
              if ( j % 5 == 0 || j == n ) fprintf(prou,"\n ");
            }
            }
        }
        }
    }
        
        return;
        
    case 16:
      if (!fsilent){
        fprintf(prou,"exit from full SQP\n");
        fprintf(prou,"            termination reason  %7.0e\n", 
        accinf[itstep][30]);
        fprintf(prou,"          final value of tauqp  %10.3e\n",
        accinf[itstep][31]);
        fprintf(prou,"      sum norm of slack vector  %10.3e\n",
        accinf[itstep][32]);
                                
        fprintf(prou,"\n\n  phase=%3i  scf0= %11.4e\n",phase,scf0);
        fprintf(prou,"\n\n  d = \n");
        for (i = 1 ; i <= n ; i++) {
            fprintf(prou,"  %11.4e", d[i]);
            if ( i % 6 == 0 || i == n ) fprintf(prou,"\n");
        }
        if ( nres != 0 ) { 
            fprintf(prou,"\n  multipliers: first estimate\n  u =\n");
            for (k = 1 ; k <= 2*nres ; k++) {
                fprintf(prou," %4i  %11.4e", k,u[k]);       
                if ( k % 4 == 0 || k == 2*nres ) fprintf(prou,"\n");
            }
        }
      }
      if ( intakt ) {
           Rprintf(  "exit from full SQP\n");
           Rprintf(  "            termination reason  %7.0e\n", 
            accinf[itstep][30]);
           Rprintf(  "          final value of tauqp  %10.3e\n",
            accinf[itstep][31]);
           Rprintf(  "      sum norm of slack vector  %10.3e\n",
            accinf[itstep][32]);
                                
           Rprintf("\n\n  phase=%3i  scf0= %11.4e\n",  phase,scf0);
           Rprintf("\n\n d = \n");
            for (i = 1 ; i <= n ; i++) {
               Rprintf(  "  %11.4e", d[i]);
                if ( i % 6 == 0 || i == n )Rprintf(  "\n");
            }
            if ( nres != 0 ) {
               Rprintf(  "\n  multipliers: first estimate\n  u =\n");
                for (k = 1 ; k <= 2*nres ; k++) {
                   Rprintf(  " %4i  %11.4e", k,u[k]);
                    if ( k % 4 == 0 || k == 2*nres )Rprintf(  "\n");
                }
            }
      }
      return;
        
    case 17:
      if (!fsilent) fprintf(prou,"small directional derivative %.15e: finish\n",dirder);
      if ( intakt )
      Rprintf(      "small directional derivative %.15e: finish\n",dirder);
        
      return;
        
    case 18:
        if ( intakt )
      Rprintf(      "small correction from full regularized SQP,finish\n");
        if (!fsilent)
      fprintf(prou,"small correction from full regularized SQP,finish\n");
        
        return;
        
    case 19:
        if (!fsilent)fprintf(prou, "QP-solver terminated unsuccessfully\n");
        if ( intakt )Rprintf("QP-solver terminated unsuccessfully\n");
        
        return;
        
    case 20:
        if ( intakt )Rprintf("restart with scaled unit matrix\n");
        if (!fsilent)fprintf(prou,        "restart with scaled unit matrix\n");
        
        return;
        
    case 21:
    
        return;
        
    case 22:
    
        return;
    }
}

/* **************************************************************************** */
/*         computation of new scaling factors for L1-penalty-function           */
/* **************************************************************************** */
void o8sce(void) {

    void    o8info(int icase);
    
    static int  i;
    static double   term,s1,s2,diff0;
    static int  wlow;

    wlow = FALSE;
    for (i = 1 ; i <= 2*nres ; i++) {
    
        /* w1 tentative new weights */
        
        term = ny*fabs(u[i])+tau;
        if ( term > w[i] ) {
            w1[i] = term+tau;
        } else {
            w1[i] = w[i];
            if ( term < w[i]*p5 && o8bind[i] == 1 ) w1[i] = (term+w[i])*p5;
        }
        if ( w1[i] < w[i]) wlow = TRUE;
    }
    /* wlow equals TRUE if one tentative weight at least has been decreased */
    
    s1 = zero;
    s2 = zero;
    for (i = 1 ; i <= nres ; i++) {
        if ( low[i] == up[i] ) {
/*  an equality constraint counts only once whereas an interval constraint */
/*  gives two inequality constraints                                       */ 
   
            s1 = s1+w1[2*i-1]*fabs(resst[2*i-1]);
            s2 = s2+w1[2*i-1]*fabs(res[2*i-1]);
        } else {
            s1 = s1-min(zero,resst[2*i-1])*w1[2*i-1];
            s2 = s2-min(zero,res[2*i-1]  )*w1[2*i-1];
            s1 = s1-min(zero,resst[2*i])*w1[2*i];
            s2 = s2-min(zero,res[2*i]  )*w1[2*i];

        }
    }
    diff0 = (fxst-fx)*scf+(s1-s2);
    if ( wlow && diff0 >= eta*clow && itstep-lastdw > max(5,min(20,n/10)) ) 
    {
    
        /* accept new (diminished ) weights */
        
        if ( clow > itstep/10 ) {
            eta = onep3*eta;
            if ( ! silent ) o8info(11);
        }
        lastch = itstep;
        lastdw = itstep;
        level  = diff0/iterma;
        psist  = s1;
        psi    = s2;
        for (i = 1 ; i <= 2*nres ; i++) {
            w[i] = w1[i];
        }
        clow = clow+one;
    } 
    else 
    {
    
        /* increase individual weights if necessary. let weigths unchanged */
        /* otherwise                                                       */
        
        s1 = zero;
        s2 = zero;
        for (i = 1 ; i <= nres ; i++) {
            if ( w1[2*i-1] > w[2*i-1] || w1[2*i] > w[2*i] ) {
                lastup = itstep;
                lastch = itstep;
            }
            w[2*i-1] = max(w[2*i-1],w1[2*i-1]);
            w[2*i]   = max(w[2*i],w1[2*i]);
            if ( low[i] == up[i] ) {
                s1 = s1+w[2*i-1]*fabs(resst[2*i-1]);
                s2 = s2+w[2*i-1]*fabs(res[2*i-1]);
            } else {
                s1 = s1-w[2*i-1]*min(zero,resst[2*i-1]);
                s2 = s2-w[2*i-1]*min(zero,res[2*i-1]);
                s1 = s1-w[2*i]*min(zero,resst[2*i]);
                s2 = s2-w[2*i]*min(zero,res[2*i]);
            }
        }
        psist = s1;
        psi   = s2;
    }   
    term = zero;
    if ( nres >= 1 ) term = w[1];
    for (i = 2 ; i <= 2*nres ; i++) {
        term = max(term,w[i]);
    }
    accinf[itstep][20] = term;
    
    /* maximum of weights */
    
    accinf[itstep][19] = clow;
    
    if ( ! silent ) o8info(12);
    
    return;
}

/* **************************************************************************** */
/*          computation of the Pantoja-Mayne BFGS-update of hessian             */
/* **************************************************************************** */
void o8bfgs(void) {

    void    o8msg (int num);
    void    o8inim(void);
    double  o8sc1 (int i,int j,double a[],double b[]);
    double  o8sc2 (int n,int m,int j,double **a,   double b[]);
    double  o8sc3 (int n,int m,int j,double **a,double b[]);
    double  o8sc3_(int n,int m,int j,double **a,   double b[]);
    void    o8upd(double **r,double z[],double y[],int n,int *fail);
    double  o8vecn(int nl,int nm,double x[]);

    static int  i,j,k;
    static double   den1,den2,den3,
                    th,tk,xsik,
                    term,term1,anorm,acond,ndx,ngtdx,den21;
    static int  fail;

    for ( i = 1 ;  i <= n ;  i++) {
         o8bfgs_dg[i]   = gphi1[i]-gphi0[i];
    }
    if ( o8vecn(1,n,o8bfgs_dg) == zero ) {
    
        /* suppress update */
        
        accinf[itstep][27] = zero;
        accinf[itstep][28] = zero;
        accinf[itstep][29] = zero;
        
        if ( ! silent ) o8msg(21);
        
        return;
    }
    for ( i = 1 ;  i <= n ;  i++) {
        
        /* multiply dx = (s in the usual notation) by Cholesky-factor */
        /* stored in the upper half of a                              */
         
         o8bfgs_ltdx[i] = o8sc2(i,n,i,a,difx);
    }
    for (i = 1 ; i <= n ; i++) {
        /* multiply by Cholesky factor transpose : A=L*L' , L' = R    */
        o8bfgs_adx[i] = o8sc3_(1,i,i,a,o8bfgs_ltdx);
    }
    /* o8bfgs_adx = a * ( x-x0), x-x0 = difx */
    
    for (i = 1 ; i <= aalist[0] ; i++) {
        j=aalist[i];
        if ( j <= 2*n )
        /* a bound constraint */
        {
          o8bfgs_gtdx[i] = xsc[(j+1)/2]*difx[(j+1)/2];
          if ( j % 2 == 0 ) o8bfgs_gtdx[i] = -o8bfgs_gtdx[i];
        }
        else
        {
          j = ( j - 2*n +1)/2;
          o8bfgs_gtdx[i] = o8sc3(1,n,j,gres,difx)*gres[0][j];
          o8bfgs_gtdx[i] = o8bfgs_gtdx[i]/gresn[j];
        }
    }
    /* o8bfgs_gtdx = grad(res(alist))(transpose)*(x-x0) */
    
    ndx   = o8vecn(1,n,difx);
    tk    = min(p5,pow(o8dnorm,2));
    anorm = zero;
    term1 = fabs(a[1][1]);
    anorm = zero;
    for (i = 1 ; i <= n ; i++) {
        for (j = i ; j <= n ; j++) {
            anorm = anorm+pow(a[i][j],2);
        }
        term1 = min(term1,fabs(a[i][i]));
    }
    if ( term1 != zero ) {
        acond = anorm/pow(term1,2);
    } else {
        acond = epsmac/tolmac;
    }
    den1 = pow(o8vecn(1,n,o8bfgs_ltdx),2);
    den2 = o8sc1(1,n,o8bfgs_dg,difx);
    if ( den1 <= rho1*anorm*pow(ndx,2) || acond >= one/rho1 ) {
    
        /* take a restart step , since condition estimate indicates */
        /* near singularity                                         */        
        o8inim();
        
        return;
    }
    if ( nr == 0 ) {
        /* formerly this was nres == 0 , but now we have always nres>=n */    
        /* in the unconstrained case we take the Powell update          */
        
        th = one;
        if ( den2 < p2*den1 ) {
            th = p8*den1/(den1-den2);
            for (i = 1 ; i <= n ; i++) {
                o8bfgs_dg[i] = th*o8bfgs_dg[i]+(one-th)*o8bfgs_adx[i];
            }
            den2 = o8sc1(1,n,o8bfgs_dg,difx);
        }
        term = one/sqrt(den2);
        for (i = 1 ; i <= n ; i++) {
            o8bfgs_dg[i]   = o8bfgs_dg[i]*term;
            o8bfgs_updz[i] = o8bfgs_dg[i];
        }
        term = one/sqrt(den1);
        for (i = 1 ; i <= n ; i++) {
            o8bfgs_updx[i] = o8bfgs_adx[i]*term;
        }
        accinf[itstep][28] = den2/den1;
        accinf[itstep][29] = th;
        accinf[itstep][27] = two;
        if ( th != one ) accinf[itstep][27] = three;
    } else {
        ngtdx = o8vecn(1,aalist[0],o8bfgs_gtdx);
        term  = one/sqrt(den1);
        for (i = 1 ; i <= n ; i++) {
            o8bfgs_updx[i] = o8bfgs_adx[i]*term;
        }
        if ( den2 >= rho1*o8sc1(1,n,o8bfgs_dg,o8bfgs_dg)
        && o8vecn(1,n,o8bfgs_dg) >= sqrt(epsmac)*ndx ) {
            xsik = zero;
            for (i = 1 ; i <= n ; i++) {
                o8bfgs_updz[i] = o8bfgs_dg[i];
            }
            den21 = den2;
        } else {
        
            /* try Pantoja-Mayne modification */
            
            den3 = tk*pow(ndx,2)+pow(ngtdx,2);
            if ( den2 >= rho1*o8sc1(1,n,o8bfgs_dg,o8bfgs_dg) ) {
                xsik = one;
            } else {
                xsik = one+(tk*pow(ndx,2)+fabs(den2))/den3;
            }
            for (i = 1 ; i <= n ; i++) {
                term = zero;
                for (j = 1 ; j <= aalist[0] ; j++) 
                {
                  k = aalist[j];
                  if ( k > 2*n )
                  { 
                    k = ( k -2*n+1)/2 ;
                    term1 = gres[i][k]*o8bfgs_gtdx[j]*gres[0][k];
                    term1 = term1/gresn[k];
                    term  = term+term1;
                  }
                  else
                  {
                    term1 = zero ;
                    if ( i == (k+1)/2 ) term1 = xsc[i]*o8bfgs_gtdx[j] ;
                    if ( k % 2 == 0 ) term1 = -term1 ;
                    term += term1 ;
                  }
                }
                o8bfgs_updz[i] = o8bfgs_dg[i]+xsik*(tk*difx[i]+term);
            }
            den21 = o8sc1(1,n,o8bfgs_updz,difx);
        }
        /*  theoretically den21>0. if not, everything is spoiled by roundoff */
        /*  do nothing then                                                  */
        if ( den21 <= zero ) {
          return;
        }
        term = one/sqrt(den21);
        for (i = 1 ; i <= n ; i++) {
            o8bfgs_updz[i] = o8bfgs_updz[i]*term;
        }
        th = one;
        if ( den2 < p2*den1 ) {
            th = p8*den1/(den1-den2);
            for (i = 1 ; i <= n ; i++) {
                o8bfgs_dg[i] = th*o8bfgs_dg[i]+(one-th)*o8bfgs_adx[i];
            }
            den2 = o8sc1(1,n,o8bfgs_dg,difx);
        }
        term = one/sqrt(den2);
        for (i = 1 ; i <= n ; i++) {
            o8bfgs_dg[i] = o8bfgs_dg[i]*term;
        }
        if ( o8vecn(1,n,o8bfgs_dg) <= tm3*o8vecn(1,n,o8bfgs_updz) ) {
        
            /* the Powell update produces a smaller growth */
            
            for (i = 1 ; i <= n ; i++) {
                o8bfgs_updz[i] = o8bfgs_dg[i];
            }
            accinf[itstep][28] = den2/den1;
            accinf[itstep][29] = th;
            accinf[itstep][27] = two;
            if ( th != one ) accinf[itstep][27] = three;
        } else {
        
            /* no update if strongly irregular */
            
            accinf[itstep][27] = one;
            accinf[itstep][28] = tk;
            accinf[itstep][29] = xsik;
        }
    }
    o8upd(a,o8bfgs_updz,o8bfgs_updx,n,&fail);
    
    /* check illconditioning after updating */
    
    term  = fabs(a[1][1]);
    term1 = term;
    i     = 1;
    
    /* in order to overcome a curious error in hp's f77 compiler */
    /* this kind of loop                                         */
    
    while ( i < n ) {
        i     = i+1;
        term  = max(term, fabs(a[i][i]));
        term1 = min(term1,fabs(a[i][i]));
    }
    if ( fail || pow(term1,2) <= rho1*pow(term,2) ) {
    
        /* reset */
        
        o8inim();
    }
    return;
}

/* **************************************************************************** */
/*                      write short information on standard out                 */
/* **************************************************************************** */
void o8shms(void) {

    static double   umin;

    if ( te0 && ! silent ) {
       umin = accinf[itstep][11];
       if (intakt)
     Rprintf(
         "%5i fx= %14.6e upsi= %8.1e b2n= %8.1e umi= %8.1e nr%4i si%2i\n",
         itstep,fx,upsi,b2n,umin,nr,(int)accinf[itstep][10]);
        
       if (!fsilent)
     fprintf(prou,
         "%5i fx= %14.6e upsi= %8.1e b2n= %8.1e umi= %8.1e nr%4i si%2i\n",
         itstep,fx,upsi,b2n,umin,nr,(int)accinf[itstep][10]);
    }
    return;
}

/* **************************************************************************** */
/*                      write messages on "special events"                      */
/* **************************************************************************** */
void o8msg(int num) 
{

    static int  i;

    if ( num <= 0 || num  > 26 ) return;
    if (silent || fsilent) return;
    
    switch (num) {
    
    case 1:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"rankdeficiency of grad's of active constr.!\n");
        fprintf(meu,"on the basis of delmin!\n");
        
        return;
        
    case 2:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"rescaling of objective function scf= %.15e\n",scf);
        
        return;
        
    case 3:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"rankdeficiency of grad's of active constr.!\n");
        fprintf(meu," del= %.15e\n", del);
        
        return;
        
    case 4:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"rankdeficiency of grad's of active constr.!\n");
        fprintf(meu," del= %.15e\n", del);
        
        return;
        
    case 5:
        fprintf(meu,"qpterm<0: no dir. of. desc., tauqp max\n");
        
        return;
        
    case 6:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"second order correction suppressed! \n");
        
        return;
        
    case 7:

        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"retry next step with a=id \n");
        
        return;
        
    case 8:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"some constraint has gradient equal to zero \n");
        fprintf(meu,"resulting d may be no direction of descent\n");
        
        return;
        
    case 9:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"try regularized SQP with increased weights\n");
        fprintf(meu,"since dnorm small or infeasibility large\n");
        
        return;
        
    case 10:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"QPsolver did not complete, try d anyway, may fail\n");
        
        return;
        
    case 11:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"direct. deriv. positive! may be due to inaccurate\n");
        fprintf(meu,"gradients or extreme illconditioning\n");
        fprintf(meu,"dirder=  %.15e\n", dirder);
        
        return;
        
    case 12:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"call of matdru suppressed, mat too large\n");
        
        return;
        
    case 13:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"startvalue corrected in order to fit bounds\n");
        
        return;
        
    case 14:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"try full SQP due to slow progress in x \n");
        
        return;
        
    case 15:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"try full SQP due to small stepsizes while\n");
        fprintf(meu,"infeasibility large\n");
    case 16:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"on exit from o8qpdu dir. deriv. positive!\n");
        fprintf(meu,"try increased tauqp\n");
        
        return;
        
    case 17:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"try regularized SQP with increased weights\n");
        fprintf(meu,"no decrease of weights possible\n");
        fprintf(meu,"and incompatibility large\n");
        
        return;
        
    case 18:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"try regularized SQP with increased weights\n");
        fprintf(meu,"since no direction of descent has been obtained\n");
        
        return;
        
    case 19:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"degeneracy in dual QP\n");
        fprintf(meu,"restr. %i to be added\n",iptr);
        fprintf(meu,"new rii= %.15e\n",riitr);
        fprintf(meu,"length of current working set=%i\n",iqtr);
        fprintf(meu,"working set\n");
        for (i = 1 ; i <= iqtr ; i++) {
            fprintf(meu,"%4i ",aitr[i]);
            if ( i % 15 == 0 || i == iqtr ) fprintf(meu,"\n");
        }
        fprintf(meu,"primal feasibility violation is= %.15e\n",sstr);
        
        return;
        
    case 20:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"dual QP seemingly infeasible \n");
        fprintf(meu,"theoretically impossible\n");
        
        return;
        
    case 21:
        fprintf(meu,"step=%i\n", itstep);
        fprintf(meu,"no update since o8bfgs_dg=0\n");
        
        return;
        
    case 22:
        fprintf(meu,"step%i\n", itstep);
        fprintf(meu,"function evaluation returns err=true\n");
        
        return;
        
    case 23:
        fprintf(meu,"step%i\n", itstep);
        fprintf(meu,"contraint evaluation returns err=true\n");
        
        return;
        
    case 24:
        fprintf(meu,"step%i\n", itstep);
        fprintf(meu,"current point cannot be changed in current\n");
        fprintf(meu,"direction due to error-message from function\n");
        fprintf(meu,"evaluation\n");
        
        return;
        
    case 25:
        fprintf(meu,"step%i\n", itstep);
        fprintf(meu,"reduce stepsize due to error-indicator set\n");
        fprintf(meu,"by users function evaluation\n");
        
        return;
    case 26:
        fprintf(meu,"step%i\n", itstep);
        fprintf(meu,"dual qp: no increase in primal objective: terminate\n");
        return;
    }
}

/* **************************************************************************** */
/* equality constrained recursive quadratic programming with                    */
/* multiple inactivation and superlinearly convergent projected                 */
/* BFGS-update (version 12/93 Spellucci, modified subsequently )                */
/* this version from 31.10.2001                                                 */
/* **************************************************************************** */
void o8opti(void) 
{
    void    o8info (int icase);
    void    o8sce  (void);
    void    o8bfgs (void);
    void    o8shms (void);
    void    o8msg  (int num);
    void    o8inim (void);
    void    o8dird (void);
    void    o8cutd (void);
    void    o8smax (void);
    void    o8unim (double sig1th);
    void    o8egph (double gphi[]);
    void    o8dec  (int nlow,int nrl);
    void    o8ht   (int id,int incr,int is1,
                    int is2,int m,
                    double **a,double beta[],double b[],double c[]);   
    void    o8sol  (int nlow,int nup,double b[],double x[]);
    void    o8solt (int nlow,int nup,double b[],double x[]);
    void    o8rght (double **a,double b[],double y[],
                    double *yl,int n);
    void    o8left (double **a,double b[],double y[],
                    double *yl,int n);
    double  o8vecn (int nl,int nm,double x[]);
    double  o8sc3 ( int i , int j , int k , 
                    double **mat, double x[] );
    double  o8sc4 ( int i , int j , int k , 
            double **mat) ;
    void    o8qpdu (void);
    void    esf    (double x[],double *fx);
    void    esgradf(double x[],double gradf[]);
    void    escon    (int type, int liste[], double x[],
               double constr[],
                int  errlist[]);
    void    escongrad( int liste[], int shift ,
            double x[], double **grad_constr);
    
    void    user_eval(double xvar[],int mode);
    void    newx ( double x[], double u[], int itstep, double **accinf,
                   int *cont ) ;

    static int  /*l,*/l0,i,j,k,csssig,csirup,csreg,cschgx;
    static int  csmdph;
    static double   delsig,delx,sum,term;
    static double   umin,term1,scfh,unorm;
    static double   compl,del1;
    static int  iumin,rank0,nr0,csdifx,clwold;
    static int  nrbas;
    static double   eps,delold,uminsc,fac,slackn,tauqp0,denom;
    static int  nperm,qpnew,etaini;
    /* added feature 25.01.2000 */
    static int  viobnd ;
    /* end added feature */
    /* new form of constraint evaluation */
    /* static int eval_err; */
    /* added feature user interrupt */
    static int cont ;
    /* initialization */
    

    for (i = 1 ; i <= n ; i++)
    {
        d[i]  = zero;
        d0[i] = zero;
    }
    itstep    = 0;
    alist[0]  = 0;      /* active general constraints */
    aalist[0] = 0 ;     /* all active constraints in 1,...,2*nres */
    o8opti_delist[0] = 0;
    violis[0] = 0;
    clist[0]  = 0;      /* the nonlinear active constraints */
    upsi      = zero;
    psi       = zero;
    psi0      = zero;
    sig0      = zero;
    d0norm    = one;
    unorm     = one;
    fx        = zero ;
    fx0       = zero ;
    /* in order to have cosphi well defined for itstep = 1 */
    
    o8dnorm  = one;
    del    = del0;
    
    /* count successive regularization steps */
    
    csreg  = 0;
    
    /* count successive small changes in x */
    
    cschgx = 0;
    
    /* count small differences of fx */
    
    csdifx = 0;
    
    /* count irregular quasi-Newton-updates */
    
    csirup = 0;
    
    /* count successive small stepsizes */
    
    csssig = 0;
    
    /* count successive small differences of penalty-function */
    
    csmdph = 0;
    /*   moved to o8st : matsc  = one;  */
    
    /* formerly tauqp = tp2 is worse */
    
    tauqp  = one;
    nperm  = FALSE;
    ident  = FALSE;
    etaini = FALSE;
    if ( n > 100 || nres > 100 ) te3 = FALSE;
    for (i = 1 ; i <= n ; i ++) 
    {
        perm[i]  = i;
        perm1[i] = i;
    }
    /*  compute first list of binding constraints */
    for ( i = 1 ; i <= nres ; i++ ) 
    {
    /*  equality constraint is always active . use the odd numbered formal 
    constraint only */     
        if ( low[i] == up[i] )
        {
          aalist[0]       += 1 ;
          aalist[aalist[0]] = 2*i-1 ;
          if ( i > n ) 
    /*  a general constraint                 */
          {
            alist[0] += 1;
            alist[alist[0]] = i-n ;
            gres[0][i-n] = one ;
          }
          if ( i > n+nlin )
          {
    /*  a nonlinear equality constraint                             */
            clist[0] += 1;
            clist[clist[0]] = i-n-nlin ;
          }

          o8bind[2*i-1]     = 1 ;
          o8bind[2*i]       = 0 ;
          o8bind0[2*i-1]    = 1 ;
          o8bind0[2*i]      = 0 ;
        }
    }
    if ( analyt ) 
    {
        eps = min(epsx,sqrt(epsmac));
    } 
    else 
    {
        eps = epsdif;
        if ( epsx < pow(epsdif,2) ) epsx = pow(epsdif,2);
    }
    eps = max(epsmac*tp3,min(tm3,eps));
    
    /* calling for external function evaluation necessary only if corr = TRUE */

    /* function and gradient values, from xtr = x*xsc */

    /*  remember that that es_routines consider bloc/non bloc call           */
    /*  in case of bloc == TRUE the evaluation has been done in o8st already */ 

    for ( i = 1 ; i <= n ; i++ )
    { res[2*i-1] = x[i]-ug[i];      /*  the scaled variables */
      res[2*i]   = og[i]-x[i] ;
    }
    for ( i = 1 ; i<=nlin ; i++ )
    {
      term = o8sc3(1,n,i,gres,x);  /*   the gradients of the linear         */
                                   /*   constraints are scaled accordingly  */
                                   /*   in o8st already                     */
      res[2*(n+i)-1] = term - low[i+n] ;
      res[2*(n+i)]   = up[i+n] - term ;
      cres[i] += 1 ;
    } 
    /*  ------- >  evaluate _all_ nonlinear constraints                     */
    for ( i = 1 ; i <= nonlin ; i++ )
    {
       confuerr[i] = FALSE ;
    }
    escon(1,clist,x,o8opti_con,confuerr);
    /*  1. parameter = 1 makes second parameter meaningless                 */
    /*                                                                      */
    
    /*  this evaluates the nonlinear constraints at x */
    for ( i = 1 ; i <= nonlin ; i++ ) cres[i+nlin] += 1 ;
    for ( i = 1 ; i <= nonlin ; i++ )        
    {
      if ( confuerr[i] ) 
      {
        if ( ! silent ) o8msg(23);
        optite = -10;
        return;
      }
    }
    for ( i = 1 ; i <= nonlin ; i++ ) 
    { 
      res[2*(i+n+nlin)-1] = o8opti_con[i]-low[i+n+nlin] ;
      res[2*(i+n+nlin)] =   up[i+n+nlin]-o8opti_con[i] ;    
    }
    for ( i =1 ; i <= nres ; i++ )
/* check for active inequality constraints and compute the scaled and */
/* the unscaled penalty function                                      */
    {
       if ( low[i] == up[i] ) 
       {
          term  = fabs(res[2*i-1]);
          term1 = zero ;
       }
       else
       {
          term =  - min(zero,res[2*i-1]);
          term1=  - min(zero,res[2*i]);
          if ( res[2*i-1] <= delmin )
          {
            aalist[0]       += 1 ;
            aalist[aalist[0]] = 2*i-1 ;
            o8bind[2*i-1]     = 1 ;
          }
          if ( res[2*i] <= delmin )
          {
            aalist[0]       += 1 ;
            aalist[aalist[0]] = 2*i ;
            o8bind[2*i]       = 1 ;
          }
          if ( o8bind[2*i-1]+o8bind[2*i] == 2 )
          {
            REprintf(" donlp2: lower and upper bound are binding ");
            REprintf(" decrease delmin !\n ");
            error("Rdonlp2 Internal Error");
          }
          if ( i > n  && (o8bind[2*i-1]+o8bind[2*i] == 1 ) )
          {
            alist[0] += 1 ;
            alist[alist[0]] = i-n ;
            if ( o8bind[2*i-1] == 1 )
            {
              gres[0][i-n] = one; 
            }
            else
            {
              gres[0][i-n] = - one ;
            }
          }
          if ( i > n+nlin  && (o8bind[2*i-1]+o8bind[2*i] == 1 ) )
          {
            clist[0] += 1 ;
            clist[clist[0]] = i-n-nlin ;
          }
  
          
       }
/* because of the definition of delmin the lower and the upper bound cannot */
/* be active simultaneously                                                 */
       upsi += term+term1 ;
       psi  += w[2*i-1]*term+w[2*i]*term1 ;
    }
    /* only the gradients of the nonlinear constraints need to be evaluated     */
    /* they are stored in gres[][nlin+clist[]]                                  */
    escongrad(clist,nlin,x,gres);
    for ( i = 1 ; i <= clist[0] ; i++ )
    {
       cgres[clist[i]+nlin] += 1 ;
       val[clist[i]+nlin] = TRUE ;
       gresn[clist[i]+nlin] = max ( one , sqrt(o8sc4(1,n,clist[i]+nlin,gres)) );
    }

    /* end first evaluation of all contraints */
    L100:

    /* ******************************************** */
    /* obtaining a point feasible within tau0 first */
    /* ******************************************** */
    
    if ( upsi >= tau0 ) 
    {
        scf   = zero;
        phase = -1;
    } 
    else 
    {
        ffuerr = FALSE;
        
        esf(x,&fx);
        icf += 1 ;
        if ( ffuerr ) 
        {
            if ( ! silent ) o8msg(22);
            optite = -9;
            
            return;
        }
        if ( ! val[0] ) 
        {
        
            /* we assume that the gradient evaluation can be done whenever */
            /* the function has been evaluated                             */
            
            esgradf(x,gradf);
            icgf += 1 ;
            val[0] = TRUE;
        }
        scf    = one;
        phase  = 0;
        fxst   = fx;
        psist  = psi;
        upsist = upsi;
        for (j = 1 ; j <= 2*nres ; j++) 
        {
            resst[j] = res[j];
        }
        eta = zero;
    }
    L200:

    /* *************************************** */
    /* main iteration loop: getting a better x */
    /* *************************************** */
    if ( itstep > 0 ) 
    {    
       cont = FALSE ;
       newx( x , u , itstep , accinf , &cont );
       if ( ! cont )
       { 
         optite = 8 ;
         return ;
       }
    }
    if ( ! ident ) 
    {
        itstep = itstep+1;
        if ( itstep > iterma ) 
        {
            optite = -three;
            itstep = iterma;
            b2n    = accinf[itstep][8];
            b2n0   = accinf[itstep][7];
            
            return;
        }
        qpnew  = FALSE;
        qpterm = 0;
        delold = del;
        del    = zero;
        b2n0   = -one;
        b2n    = -one;
        singul = FALSE;
        nperm  = FALSE;
        for (i = 1 ; i <= n ; i++) 
        {
            nperm   = nperm || (perm[i] != perm1[i]);
            perm[i] = perm1[i];
            diag[i] = zero;
        }
    }
    /* ********************************************************* */
    /* current valid row permutation for QR-decomposition of     */
    /* matrix of binding gradients in order to obtain continuity */
    /* of the QR-decomposition is given by perm                  */
    /* ********************************************************* */
        
    nr    = aalist[0];
    nrbas = nr;
    for (i = 1 ; i <= 2*nres ; i++) 
    {
        o8opti_bindba[i] = o8bind[i];
    }
    for (j = 1 ; j <= 32 ; j++) 
    {
        accinf[itstep][j] = zero;
    }
    gfn = o8vecn(1,n,gradf);
    
    /* compute new weight of objective function if useful */
    
    if ( nres > 0 && phase >= 0 && ! ident && itstep > 1 && 
        ( (accinf[itstep-1][10] == -1. && scf0 == one )
        || accinf[itstep-1][10] == 1. ) ) 
    {
        
        /* try rescaling the objective function */
    
        term = zero;
        for (i = 1 ; i <= nlin+nonlin ; i++) 
        {
             term = max(term,gresn[i]);
        }
        scfh = term/max(one/scfmax,gfn);
        if ( scfh < one/scfmax ) scfh = one/scfmax;
        if ( scfh > scfmax     ) scfh = scfmax;
        if ( (fxst-fx)*scfh+scfh/scf*(psist-psi) >= 
            scfh/scf*eta*clow && lastch <= itstep-4 
            && (scfh < tm1*scf || scfh > tp1*scf) )
        {
            
            /* rescale the objective function if this seems promising and the */
            /* change is significant                                          */
    
            clow  = clow+1;
            term  = scfh/scf;
            psi   = psi*term;
            psist = psist*term;
            for (i = 1 ; i <= 2*nres ; i++) 
            {
                u[i] = u[i]*term;
            }
            unorm  = unorm*term;
            scf    = scfh;
            lastch = itstep;
            term   = sqrt(term);
            for (i = 1 ; i <= n ; i++) 
            {
                diag0[i] = term*diag0[i];
                for (j = 1 ; j <= n ; j++) 
                {
                    a[j][i] = a[j][i]*term;
                }
            }
            matsc = matsc*term;
            if ( ! silent ) o8msg(2);
        }
    }
    accinf[itstep][1] = itstep;
    accinf[itstep][2] = fx;
    accinf[itstep][3] = scf;
    accinf[itstep][4] = psi;
    accinf[itstep][5] = upsi;
    
    if ( ! silent ) o8info(1);

    /* begin solver */
    
    /* *********************************************** */
    /* QR-decomposition of matrix of binding gradients */
    /* *********************************************** */
    
    if ( nr >= 1 ) 
    {
        o8dec(1,nr);
    } 
    else 
    {
        rank = 0;
    }
    o8left(a,gradf,o8opti_yy,&term,n);
    
    for (i = 1 ; i <= n ; i++) 
    {
        qgf[i] = o8opti_yy[perm[i]];
    }
    o8ht(1,0,1,rank,n,qr,betaq,qgf,o8opti_trvec);
    
    for (i = 1 ; i <= n ; i++) 
    {
        qgf[i] = o8opti_trvec[i];
    }
    if ( rank != nr && ! silent ) o8msg(1);
    
    /* ******************************************************* */
    /* compute del as function of x (forcing infeasibility and */
    /* the projected gradient to zero)                         */
    /* ******************************************************* */
    
    b2n0 = o8vecn(rank+1,n,qgf);
    
    sum  = zero;
    for (i = 1 ; i <= nres ; i++) 
    {
        if ( low[i]==up[i] ) 
        {
            term = fabs(res[2*i]);
            if ( i > n ) term /= gresn[i-n];
            sum += term;
        } 
        else 
        {
           term = -min(zero,res[2*i-1]);
           term1 = -min(zero,res[2*i]);
           if ( i > n )
           {
              term /= gresn[i-n];
              term1 /= gresn[i-n];
           } 
           sum += term + term1;
        }
    }
    /*  sum is unweighted  infeasibility, using scaled constraints */    
    if ( itstep > 1 && accinf[itstep-1][8] >= zero 
        && ! etaini && accinf[itstep-1][18] >= 0 ) 
    {
        etaini = TRUE;
        eta    = (accinf[itstep-1][8]/max(one,gfn)+sum
                 +min(one,slackn)+min(one,fabs(uminsc)))/min(30*n,iterma);
        level  = eta;
    }
    if ( itstep > 1 ) 
    { 
        delx = delmin;
    }
    else
    {
        delx=del01;
    } 
    term = scf*(fx0-fx)+psi0-psi;
    if ( itstep > 1 && term > zero && scf != zero ) 
       delx = max(delx,exp(p7*p7*log(term)));
    if ( scf == zero ) delx = min(del0*tm4,max(delx,upsi*tm2));
    delsig = delmin;
    
    /* del should be large enough to include constraints hit in */
    /* step before violis comes from unimin                     */
    
    for (i = 1 ; i <= violis[0] ; i++) 
    {
        j      = violis[i];
        denom = ( (j+1)/2 > n ) ? gresn[(j+1)/2-n] : xsc[(j+1)/2] ;
        delsig = max(delsig,res[j]/denom*(one+tm1));
    }
    del = min(del0,max(min(delsig,five*delx),delx));


    if ( violis[0] == 0 ) del = min(del,del01);
    
    /* ********************************************* */
    /* if phase = 2 don't loose a binding constraint */
    /* phase = 2 implies o8opti_delist[0] = 0               */
    /* ********************************************* */
    
    if ( phase == 2 && violis[0] == 0 ) 
    {
        for (i = 1 ; i <= nres ; i++) 
        {
            if ( !( low[i]==up[i]) ) 
            {
              term = zero ;
              if ( o8bind0[2*i-1] == 1 ) term=fabs(res[2*i-1]);
              if ( o8bind0[2*i] == 1 ) term=fabs(res[2*i]);
              if ( i > n ) term /= gresn[i-n];
              else term /= max(one,xsc[i]);
              
              del = min(del01,max(del,term));
            }
        }
    }

    term = del;
    for (i = 1 ; i <= o8opti_delist[0] ; i++) 
    {
        j     = o8opti_delist[i];
        denom = ( (j+1)/2 > n ) ? gresn[(j+1)/2-n] : xsc[(j+1)/2];
        term1 = res[j]/denom*(one-tm2);
        if ( term1 >= del*tm2 ) term = min(term,term1);
    }
    del = term;
    
    /* if del becomes too large, we may loose complementary slackness */
    
    if ( itstep > 1 && ! ident && scf != zero ) 
    {
        compl = zero;
        for (i = 1 ; i <= nres ; i++) 
        {
            if ( !(low[i] == up[i] ) )
            { 
               term = res[2*i-1];
               term1 = res[2*i];
               if ( i > n ) 
               {
                 term /= gresn[i-n];
                 term1 /= gresn[i-n];
               }
               else
               {
                 term /= max(one,xsc[i]);
                 term1 /= max(one,xsc[i]);
               }
               term = max ( zero , term-delmin );
               term1 = max ( zero , term1 - delmin ) ;
               term *= max(zero,u[2*i-1]-smallw);
               term1 *= max(zero,u[2*i]-smallw);
               if ( i > n )
               {
                 term /= gresn[i-n];
                 term1 /= gresn[i-n];
               }
               else
               {   
                 term /= max(one,xsc[i]);
                 term1 /= max(one,xsc[i]);
               }
 
               compl += term+term1 ;
            }
        }
        if ( compl > zero ) 
        {
           for (i = 1 ; i <= nres ; i++) 
           {
              if ( !(low[i] == up[i]) ) 
              { 
                term = res[2*i-1];
                term1 = res[2*i];
                if ( i > n )
                {
                  term /= gresn[i-n];
                  term1 /= gresn[i-n];
                }           
                else
                {   
                 term /= max(one,xsc[i]);
                 term1 /= max(one,xsc[i]);
                }

                if ( u[2*i-1] > smallw && term > tp2*delmin ) 
                {
                    del = max(delmin*tp2,
                              min(del,term*(one-tm2)));
                }
                if ( u[2*i] > smallw && term1 > tp2*delmin )
                {
                    del = max(delmin*tp2,
                              min(del,term1*(one-tm2)));
                }
              }
           }
        }
    }
    /* if the current step was singular and not successful,try a greater del */
    /* the same, if qpterm in the last step did signal trouble, but          */
    /* stepsize selection was nevertheless successful                        */
    
    if ( itstep > 1 && accinf[itstep-1][30] < 0. ) del = min(tp1*delold,del0);
    
    /* ********************************************* */
    /* include nearly binding inequality constraints */
    /* ********************************************* */
    clist[0] = 0 ;
    /* clist is the list of nonlinear constraints to be included additionally */
    for (i = 1 ; i <= nres ; i++) 
    {
       if ( !(low[i] == up[i] ) )
       {
        term = res[2*i-1];
        term1 = res[2*i];
        if ( i > n )
        {
          term /= gresn[i-n];
          term1 /= gresn[i-n];
        }
        else
        {
          term /=max(one,xsc[i]);
          term1 /=max(one,xsc[i]);
        } 
        if ( min(term,term1) <= del && (o8bind[2*i-1]+o8bind[2*i] ==0 ) )
        {
          j=2*i-1;
          if ( term1 < term ) j=2*i;
          /* it may be useful to include constraints>0 if near its boundary */
          /* but avoid it, if the constraint was in the old delete-list     */
          /* constraint j is a candidate for inclusion in the active set    */
          /* check previous delete_list                                     */
/* inactive  
          for ( k = 1 ; k <= o8opti_delist[0] ; k++ )
          {
             if ( j == o8opti_delist[k] ) j = -1;
          }
end inactive */
          if ( j >= 1 )   
          /*  that means the value of j was not found in o8opti_delist */
          {
             o8bind[j]         = 1;
             aalist[0]       += 1;
             aalist[aalist[0]] = j;
             if ( j > 2*n )
             {
               alist[0] += 1 ;
               k = (j-2*n+1)/2 ;
               alist[alist[0]] = k ;
               if ( j % 2 == 0 ) 
               {
                 gres[0][k] = -one ;
               }
               else
               {
                 gres[0][k] = one ;
               }
             }
             if ( j > 2*(n+nlin) )
             {
               clist[0]       += 1 ;
               clist[clist[0]] = i-n-nlin ;
             }
          }
        }
       }
    }
    if ( clist[0] > 0 )
    {
       escongrad(clist,nlin,x,gres);
       for ( i = 1 ; i <= clist[0] ; i++ )
       {
         cgres[clist[i]+nlin] += 1 ;
         val[clist[i]+nlin]    = TRUE ; 
         gresn[clist[i]+nlin] = max(one,sqrt(o8sc4(1,n,clist[i]+nlin,gres)));
       }
     
    }

    rank0 = rank;
    nr0   = nr;
    nr    = aalist[0] ;
    
    o8dec(nr0+1,nr);
    
    if ( rank != nr && ! silent ) o8msg(3);
    
    o8ht(1,0,rank0+1,rank,n,qr,betaq,qgf,o8opti_trvec);
    
    for (i = 1 ; i <= n ; i++) 
    {
        qgf[i] = o8opti_trvec[i];
    }
    for (i = 1 ; i <= n ; i++) 
    {
        o8opti_yy[i] = -qgf[i]*scf;
    }
    for (i = 1 ; i <= n ; i++) 
    {
        yu[i] = zero;
    }
    /* ********************************************* */
    /* first computation of lagrangian multipliers u */
    /* ********************************************* */
    
    o8sol(1,rank,o8opti_yy,yu);
    
    umin  = zero;
    unorm = zero;
    for (i = 1 ; i <= 2*nres ; i++) 
    {
        u[i] = zero;
    }
    iumin  = 0;
    uminsc = zero;
    for (i = 1 ; i <= rank ; i++) 
    {
        unorm = max(unorm,fabs(yu[i]));
        k     = aalist[colno[i]];
        u[k]  = -yu[i];
        term = one ;
        if ( k > 2*n ) term=gresn[(k-2*n+1)/2];
        if ( !(low[(k+1)/2] == up[(k+1)/2]) ) 
        {
            if ( -yu[i]/term < uminsc ) 
            {
                iumin  = k;
                uminsc = -yu[i]/term;
            }
        }
    }
    if ( scf != zero ) 
    {
        for (i = 1 ; i <= n ; i++) 
        {
            o8opti_yx[i] = scf*gradf[i];
            for (j = 1 ; j <= nlin+nonlin ; j++) 
            {
                o8opti_yx[i] = o8opti_yx[i]-gres[i][j]*(u[2*j-1+2*n]-u[2*j+2*n]);
            }
        }
        for (i = 1 ; i <= n ; i++ )
        {
                o8opti_yx[i] -= xsc[i]*(u[2*i-1]-u[2*i]) ;
        } 
    /*  the l2-norm of the original unscaled gradient of the lagrangian */        
        b2n = o8vecn(1,n,o8opti_yx)/scf;
    } 
    else 
    {
        b2n = -one;
    }
    if ( ! silent ) o8info(2);
    if ( ! silent ) o8info(3);
    
    /* compute new delta */

    del1 = del;
    if ( b2n > zero ) del1 = max(del,tm1*min(del0,
    exp(p7*log( fabs(b2n)/(gfn+one) + max(zero,-uminsc) + sum)) ));
                
    /* exclude constraints which were candidates for inactivating in the */
    /* previous step if useful                                           */
    
    for (i = 1 ; i <= o8opti_delist[0] ; i++) 
    {
        j     = o8opti_delist[i];
        term1 = res[j]*(one-tm2) ;
        if ( j > 2*n )
        {
           term1 /= gresn[(j-2*n+1)/2];
        }
        else
        {
           term1 /= max(one,xsc[(j+1)/2]);
        }
        if ( term1 >= del1*tm2 ) del1 = max(delmin,min(del1,term1));
    }

    slackn = zero;
    for (i = 1 ; i <= nres ; i++) 
    {
        term = res[2*i-1];
        term1 = res[2*i];
        denom = one ;
        if ( i > n ) denom = gresn[i-n];
        slackn += 
           max(zero,term/denom-delmin)*max(zero,u[2*i-1]-smallw)/denom
          +max(zero,term1/denom-delmin)*max(zero,u[2*i]-smallw)/denom;
    }
    if ( upsi <= delmin && b2n <= epsx*(gfn+one)
        && b2n != -one  && uminsc >= -smallw &&
        slackn <= delmin*smallw*nres ) 
    {
        
        /* sufficient accuracy in Kuhn-Tucker conditions */
    
        optite = zero;
        
        return;
    }
    /* include additional constraints if necessary */
    
    l0 = aalist[0];
    clist[0]=0;
    for (i = 1 ; i <= nres ; i++) 
    {
      if ( low[i] != up[i] )
      {
        term = res[2*i-1];
        term1 = res[2*i];
        if ( i > n ) 
        {
          term /= gresn[i-n];
          term1 /= gresn[i-n];
        }
        else
        {
          term /= max(one,xsc[i]);
          term1 /= max(one,xsc[i]);
        }
        if ( min(term,term1) <= del1 && (o8bind[2*i-1]+o8bind[2*i] == 0 ))
        {
         
            if ( term <= term1 ) 
            {
              o8bind[2*i-1]         = 1;
              aalist[0]        = aalist[0]+1;
              aalist[aalist[0]] = 2*i-1;
              if ( i > n ) 
              {
                alist[0] += 1 ;
                alist[alist[0]] = i-n ;
              }
              if ( i > n+nlin )
              {
                 clist[0] +=1 ;
                 clist[clist[0]] = i-n-nlin;
              }
            }
            else 
            {   
              o8bind[2*i]         = 1;
              aalist[0]        = aalist[0]+1;
              aalist[aalist[0]] = 2*i;
            
              if ( i > n )
              {
                alist[0] += 1 ;
                alist[alist[0]] = i-n ;
              }
              if ( i > n+nlin )
              {
                 clist[0] +=1 ;
                 clist[clist[0]] = i-n-nlin;
              } 
            } 
        }        
      } 
    } /* end for i */

    del = del1;
    accinf[itstep][6]  = del;
    accinf[itstep][7]  = b2n0;
    accinf[itstep][9]  = aalist[0];
    accinf[itstep][10] = -1.;
    nr = aalist[0];
    if ( clist[0] != 0 )
    {
       escongrad(clist,nlin,x,gres);
       for ( i = 1 ; i <= clist[0] ; i++ )
       {
         cgres[clist[i]+nlin] += 1 ;   
         val[clist[i]+nlin]    = TRUE ;
         gresn[clist[i]+nlin] = max(one,sqrt(o8sc4(1,n,clist[i]+nlin,gres)));
       }
    }
    
    if ( l0 != nr ) 
    {
        rank0 = rank;
        
        o8dec(l0+1,nr);
        
        o8ht(1,0,rank0+1,rank,n,qr,betaq,qgf,o8opti_trvec);
        
        for (i = 1 ; i <= n ; i++) 
        {
            qgf[i] = o8opti_trvec[i];
        }
    }

    if ( rank != nr ) 
    {
        if ( ! silent ) o8msg(4);
        
        goto L400;
    }
    /* ******************************************************** */
    /* second solution for multipliers, rank may have changed ! */
    /* ******************************************************** */
    
    for (i = 1 ; i <= n ; i++) 
    {
        o8opti_yy[i] = -qgf[i]*scf;
    }
    for (i = 1 ; i <= n ; i++) 
    {
        yu[i] = zero;
    }
    o8sol(1,rank,o8opti_yy,yu);
    
    /* remember the column interchanges in qr! yu[i] corresponds to */
    /* u[aalist[colno[i]]]                                          */
    
    umin  = zero;
    unorm = zero;
    for (i = 1 ; i <= 2*nres ; i++) 
    {
        u[i] = zero;
    }
    iumin  = 0;
    uminsc = zero;
    for (i = 1 ; i <= rank ; i++) 
    {
        unorm = max(unorm,fabs(yu[i]));
        k     = aalist[colno[i]];
        u[k]  = -yu[i];
        term = one ;
        term =  ( k > 2*n )? gresn[(k-2*n+1)/2] : xsc[(k+1)/2];
        if ( !(low[(k+1)/2] == up[(k+1)/2]) ) 
        {
            if ( -yu[i]/term < uminsc ) 
            {
                iumin  = k;
                uminsc = -yu[i]/term;
            }
        }

    }
    if ( scf != zero ) 
    {

        for (i = 1 ; i <= n ; i++)
        {
            o8opti_yx[i] = scf*gradf[i];
            for (j = 1 ; j <= nlin+nonlin ; j++)
            {
                o8opti_yx[i] -= gres[i][j]*(u[2*j-1+2*n]-u[2*j+2*n]);
            }
        }
        for (i = 1 ; i <= n ; i++ )
        {
                o8opti_yx[i] -= xsc[i]*(u[2*i-1]-u[2*i]) ;
        }
        b2n = o8vecn(1,n,o8opti_yx)/scf;
    }
    accinf[itstep][8]  = b2n;
    accinf[itstep][11] = umin;
    
    o8shms();
   
    
    if ( ! silent ) 
    {  
      o8info(4);
      if ( l0 != nr ) o8info(2);
    }
    
    o8opti_delist[0] = 0;
    if ( phase >= 0 && b2n != -one ) 
    {
        if ( fabs(uminsc) >= max(smallw,fabs(b2n)/(gfn+one)*c1d)) 
        {
            for (i = 1 ; i <= nr ; i++) 
            {
                k = aalist[colno[i]];
                term = one ;
                if ( k > 2*n ) term = gresn[(k-2*n+1)/2];
                if ( !(low[(k+1)/2] == up[(k+1)/2]) )
                { 
                   if ( -yu[i]/term <= -smallw) 
                   {
                     o8opti_delist[0]         = o8opti_delist[0]+1;
                     o8opti_delist[o8opti_delist[0]] = k;
                   }
                }
            }
        }
    }
    /* *********************************************************** */
    /* the new o8opti_delist doesn't influence the current d but only the */
    /* computation of the next del                                 */
    /* *********************************************************** */
    
    eqres = TRUE;
    for (i = 1 ; i <= 2*nres ; i ++) 
    {
        eqres = eqres && ( o8bind[i] == o8bind0[i] );
    }
    /* compute condition number estimators of diag-r and diag of */
    /* Cholesky-decomposition of b                               */
    
    if ( nr > 1 ) 
    {
        term  = zero;
        term1 = one;
        for (i = 1 ; i <= nr ; i++) 
        {
            term  = max(term, fabs(diag[i]));
            term1 = min(term1,fabs(diag[i]));
        }
        accinf[itstep][13] = term/term1;
    } 
    else if ( nr == 1 ) 
    {
        accinf[itstep][13] = 1.;
    } 
    else 
    {
        accinf[itstep][13] = -1.;
    }
    term  = fabs(a[1][1]);
    term1 = fabs(a[1][1]);
    i     = 2;
    while ( i <= n ) 
    {
        term  = max(term, fabs(a[i][i]));
        term1 = min(term1,fabs(a[i][i]));
        i     = i+1;
    }
    accinf[itstep][14] = pow(term/term1,2);
    
    if ( ! silent ) o8info(5);
    
    /* since a represents the Cholesky-factor, this square */
    slackn = zero;
    for (i = 1 ; i <= nres ; i++) 
    {
        term = res[2*i-1];
        term1 = res[2*i]; 
        denom = (i > n ) ? gresn[i-n] : max(one,xsc[i]);
        slackn +=
           max(zero,term/denom-delmin)*max(zero,u[2*i-1]-smallw)/denom
          +max(zero,term1/denom-delmin)*max(zero,u[2*i]-smallw)/denom;
    }

    if ( umin >= -smallw &&
        slackn <= delmin*smallw*nres &&
        upsi <= nres*delmin && upsi0 <= nres*delmin
        && fabs(fx-fx0) <= eps*(fabs(fx)+one) &&
        b2n != -one &&
        b2n <= tp2*epsx*(gfn+one) ) 
    {
        
        csdifx = csdifx+1;
    } 
    else 
    {
        csdifx = 0;
    }
    if ( phase >= 0 && (accinf[itstep][14] > tp3 || ! analyt ) && csdifx > n ) 
    {
        optite = four;
            
        /* to avoid possible slow convergence with singular    */
        /* projected hessian or inaccurate numerical gradients */
            
        return;
    }
    /* compute damping factor for tangential component if upsi>tau0/2 */

    scf0 = one;
    if ( phase >= 0 && upsi > tau0*p5 )
         scf0 = max(one/scfmax,(two*(tau0-upsi)/tau0)*upsi*tm1/max(one,gfn) )/scf;
    accinf[itstep][15] = scf0;

    /* **************************** */
    /* compute tangential component */
    /* **************************** */
    
    for (i = nr+1 ; i <= n ; i++) 
    {
        o8opti_qtx[i] = o8opti_yy[i]*scf0;
    }
    /* o8opti_qtx[nr+1],..,o8opti_qtx[n] is s2 */
    
    /* **************************************************** */
    /* compute right hand side and vertical component       */
    /* use damping for inactivation direction if very large */
    /* no indirect inactivation if infeasibility large and  */
    /* we are not almost stationary on the current manifold */
    /* **************************************************** */
    
    fac = one;
    if ( -umin*c1d > b2n+upsi && b2n != -one ) fac = c1d;
    if ( upsi > tau0*p5 ) fac = zero;
    for (i = 1 ; i <= nr ; i++) 
    {
        k    = aalist[colno[i]];
        term = res[k];
        if ( !(low[(k+1)/2] == up[(k+1)/2]) )
        {
          if (  -yu[i] < zero && term > zero ) term = -term;
          if (  -yu[i] < zero ) term = term-yu[i]*fac;
        }
        o8opti_yx[i] = -term;
    }
    o8solt(1,nr,o8opti_yx,o8opti_qtx);
    
    /* o8opti_qtx is transformed direction of descent for phi */
    
    o8ht(-1,0,1,nr,n,qr,betaq,o8opti_qtx,o8opti_yx);
    
    for (i = 1 ; i <= n ; i++) 
    {
        o8opti_qtx[perm[i]] = o8opti_yx[i];
    }
    
    /* solve l(transp)*d = o8opti_qtx, l = a = Cholesky-factor of b */
    
    o8rght(a,o8opti_qtx,d,&term,n);
    
    /* end solver */
    
    /* compute new penalty weights : regular case */
    
    clwold = clow;
    if ( phase >= 0 ) o8sce();
    if ( clow > clwold ) 
    {
    
        /* tau_qp depends on the (new) weights */
        
        term = w[1];
        for (i = 1 ; i <= 2*nres ; i++) 
        {
            term = max(term,w[i]);
        }
        tauqp = max(one,min(tauqp,term));
    }
    /* compute parameter phase and stopping criterion */

    if ( uminsc < -smallw )     phase = min(1,phase);
    if ( ! eqres )              phase = min(0,phase);
    if ( eqres && upsi < tau0 ) phase = max(1,phase);
    
    /* rescale and project d if appropriate */
    
    o8cutd();
    
    /* compute the directional derivative dirder */
    
    o8dird();
    
    /* terminate if correction is small */
    
    if ( o8dnorm <= epsx*(xnorm+epsx) && upsi <= delmin
        && b2n != -one && uminsc >= -smallw && b2n <= epsx*(gfn+one) ) 
    {
        
        optite = one;
        
        return;
    }
    L350:

    /* reenter from the singular case: dirder has been computed already */
    
    accinf[itstep][16] = xnorm;
    accinf[itstep][17] = o8dnorm;
    accinf[itstep][18] = phase;
    
    /* compute stepsize */
    
    cfincr = icf;

    /* if no descent direction is obtained, check whether restarting the method */
    /* might help                                                               */
    
    if ( dirder >= zero ) 
    {
    
        /* no direction of descent */
        
        if ( ! silent ) o8msg(11);
        stptrm = -two;
        sig    = zero;
        
        goto L360;
    }
    /* if directional derivative correct but very small, terminate */
    /* since no further progress might be possible                 */
    
    if ( -dirder <= epsmac*tp2*(scf*fabs(fx)+psi+one) )
    {
        if ( upsi > delmin*nres ) 
        {
            optite = -one;
            stptrm = -one;
        } 
        else 
        {
            optite = two;
            stptrm = one;
        }
        sig = zero;
        
        goto L360;
    }
    /* phase = 2 : we may hope to obtain superlinear convergence */
    /* switch to Maratos-correction is on then                   */
    /* return to phase = 1 if first order correction large       */
    
    if ( phase >= 1 && o8dnorm <= smalld*(xnorm+smalld) && scf0 == one &&
        uminsc >= -smallw && ! singul) phase = 2;
        
    /* return to phase 1 since correction large again */
    
    if ( phase == 2 && o8dnorm > (xnorm+smalld) ) phase = 1;

    o8smax();
    
    /* stmaxl is the maximal stepsize such that point on projected */
    /* ray changes with sigma, but sigla at most                   */
    
    for (i = 1 ; i <= n ; i++) 
    {
        dd[i] = zero;
        x1[i] = x[i]+d[i];
    }
    /* compute second order correction of infeasibility if useful    */
    /* if x1 already violates the bounds by viobnd, suppress it      */
    /* also suppress it if becomes too large compared with the first */
    /* order correction                                              */
    viobnd = FALSE ;     
    for ( i = 1 ; i<= n ; i++ ) 
    {
      if ( (llow[i] && x1[i] < ug[i]-taubnd) || 
           (lup[i] && x1[i] > og[i]+taubnd )  )
      { viobnd = TRUE ; 
      }
    }

    if ( phase == 2 && o8dnorm > sqrt(xnorm*epsmac)  && ! singul 
         && ! viobnd && nres > 0 ) 
    {
    /* compute the Maratos correction using a difference approximation */    
        if ( bloc ) 
        {
          valid = FALSE ;
          /* user_eval must reset valid */
          user_eval(x1,-1);
        }
        
        /* only function values, from xtr = xsc*x1 */
        clist[0] = 0 ; 
        for (i = 1 ; i <= aalist[0] ; i++) 
        {
        /* here only the active constraints are evaluated  and */
        /* stored in o8opti_yx                                        */
             o8opti_yx[i] = zero;
             k = aalist[i];
             if ( k > 2*(n+nlin) )
             /* for the linear constraints the function value at x1 is zero */
             /* anyway                                                      */
             {
               clist[0] += 1 ;
               clist[clist[0]] = (k+1)/2-n-nlin;
             }
        }
  /*  evaluate nonlinear active constraints from clist only         */
        for ( j = 1 ; j <= clist[0] ; j++ )
        {
           confuerr[clist[j]] = FALSE ;
           cres[clist[j]+nlin] += 1 ;
        }
        escon(2,clist,x1,o8opti_con,confuerr);
  /*                                                                */
        
        for ( j = 1 ; j <= clist[0] ; j++ )
        {
          if ( confuerr[clist[j]] ) goto L355;
        }
  /* if no error occured, computation of o8opti_yx is now to be completed */
        for ( i = 1 ; i <= aalist[0] ; i++ ) 
        {
           k = aalist[i];
           if ( k > 2*(n+nlin) )
           {
             if ( k % 2 == 0 ) 
             {
               o8opti_yx[i] =  up[(k+1)/2]-o8opti_con[(k+1)/2-n-nlin];
             }
             else
             {
               o8opti_yx[i] = o8opti_con[(k+1)/2-n-nlin]-low[(k+1)/2];
             }
           }
        }     
        for (i = 1 ; i <= aalist[0] ; i++) 
        {
            o8opti_yy[i] = -o8opti_yx[colno[i]];
        }
        o8solt(1,nr,o8opti_yy,dd);
        
        for (i = nr+1 ; i <= n ; i++) 
        {
            dd[i] = zero;
        }
        o8ht(-1,0,1,nr,n,qr,betaq,dd,o8opti_yx);
        
        for (i = 1 ; i <= n ; i++) 
        {
            dd[perm[i]] = o8opti_yx[i];
        }
        o8rght(a,dd,dd,&term,n);
        
        if ( sqrt(term) > p5*o8dnorm ) 
        {
        
            /* second order correction almost as large as first order one: */
            /* not useful                                                  */
            
            for (i = 1 ; i <= n ; i++) 
            {
                dd[i] = zero;
            }
            if ( ! silent ) o8msg(6);
        }
    }
    L355:

    if ( ! silent ) o8info(7);

    sig = min(one,stmaxl);
    
    o8unim(sig);

    L360:

    cfincr = icf-cfincr;
    if ( ! silent ) o8info(10);
    
    /* count successive small steps */
    
    term = scf*(fx0-fx)+psi0-psi;
    if ( fabs(term) <= epsmac*tp3*(scf*fabs(fx)+psi) ) 
    {
        csmdph = csmdph+1;
    } 
    else 
    {
        csmdph = 0;
    }
    /* csmdph counts contiguous small differences of penalty function phi */
    
    if ( csmdph > max(n,10) ) 
    {
        optite = seven;
        
        return;
    }
    if ( sig <= five*tm2 ) 
    {
        if ( sig0 <= five*tm2 ) csssig += 1;
    } 
    else 
    {
        csssig = 0;
    }
    /* csssig counts the number of successive small sig's */

    accinf[itstep][21] = sig;
    accinf[itstep][22] = cfincr;
    accinf[itstep][23] = dirder;
    accinf[itstep][24] = dscal;
    accinf[itstep][25] = cosphi;
    accinf[itstep][26] = violis[0];
    
    if ( sig == zero && stptrm == one && optite == two ) 
    {
    
        /* no further significant progress possible */
        
        if ( ! silent ) o8info(17);
        
        return;
    }
    if ( stptrm == one && sig <= tm4 && accinf[itstep][13] > tp4 && ! singul
        && nres > 0) 
    {
        
        /* try a regularized step, hopefully this will give a better d */
        /* and larger sig                                              */
        
        if ( accinf[itstep][14] > tp4 ) o8inim();
        ident  = TRUE;
        singul = TRUE;
        
        goto L400;
    }
    if ( stptrm < zero ) 
    {
    
        /* stepsize selection failed */
        
        if ( ! ident ) 
        {
        
            /* try restart with a = identity scaled */
            
            if ( ! silent ) o8msg(7);
            ident     = TRUE;
            o8opti_delist[0] = 0;
            violis[0] = 0;
            csreg     = 0;
            csssig    = 0;
            csirup    = 0;
            
            o8inim();
            
            aalist[0] = nrbas;
            for (i = 1 ; i <= 2*nres ; i++) 
            {
                o8bind[i] = o8opti_bindba[i];
            }
            if ( upsi >= tau0 ) 
            {
            
                goto L100;
                
            } 
            else 
            {
            
                goto L200;
            }
        }
        if ( ! singul && ident && accinf[itstep][13] > tp4 && nres > 0 ) 
        {
        
            /* try the full SQP-direction               */
            /* this may be the third try for this point */
            
            singul = TRUE;
            ident  = TRUE;
            
            goto L400;
        }
        if ( stptrm == -two ) 
        {
            optite = -four;
            
            return;
        }
        if ( sig == zero && optite == -one ) return;
        
        /* unidimensional search unsuccessfully terminated */
    
        optite = -two;
        
        return;
    }
    if ( singul && itstep > n && fabs(fx-fx0) <= eps*(fabs(fx)+one) && phase >= 0
        && upsi <= nres*delmin && upsi0 <= nres*delmin
        && slackn <= delmin*smallw*nres && infeas <= upsi && ! ident ) 
    {
        
        /* since multipliers may be incorrect for infeas != zero be careful */
    
        optite = four;
        
        /* avoid slow progress in case of singular constraints */
    
        return;
    }
    /* relaxed termination criteria in the singular case */
    
    if ( singul && upsi <= delmin*nres && upsi0 <= delmin*nres
         && b2n != -one && b2n <= (gfn+one)*epsx*tp2 && phase >= 0
         && slackn <= delmin*smallw*nres && infeas <= upsi ) 
    {
         
        /* since multipliers may be incorrect for infeas != zero be careful */
        
        optite = three;
        
        return;
    }
    k = 0;
    for (i = 1 ; i <= n ; i++) 
    {
        if ( fabs(difx[i]) >= epsx*(fabs(x[i])+tm2) ) k = 1;
    }
    if ( k == 0 ) 
    {
        cschgx = cschgx+1;
    } 
    else 
    {
        cschgx = 0;
    }
    if ( cschgx > nreset && singul ) 
    {
    
        /* very slow progress in x in the singular case. terminate */
        
        optite = five;
        
        return;
    }
    /* new value of x has been accepted */
    
    xnorm = o8vecn(1,n,x);
    ident = FALSE;
    
    o8egph(gphi0);
    
    for (i = 1 ; i <= nonlin ; i++) 
    {
      val[i+nlin] = FALSE;
    }
    val[0] = FALSE ;
    /* in bloc mode , all constraints are evaluated in the user interface */
    /* the subsequent escongrad simply is then move from fugrad to gres   */


  /* ****************************************                               */
    if ( bloc ) 
    {
      valid = FALSE ;
      /* user_eval must reset valid */
      user_eval(x,2);
      
    }
  /* ****************************************                               */

    
    /* evaluate gradients only, since function values are already */
    /* valid from unidimensional minimization                     */
    /* argument is xtr = xsc*x                                    */
    
    if ( phase >= 0  ) 
    {
        val[0] = TRUE;
        esgradf(x,gradf);
        icgf += 1;
    }
  /* evaluate gradients of nonlinear constraints  at the new point   */
  /* in case of a bloc evaluation this is simply a move from fugrad  */
    clist[0] = 0 ; 
    for ( i = 1 ; i <= aalist[0] ; i++ ) 
    {
      k = aalist[i];
      if ( k > 2*(n+nlin) )
      {
         clist[0] += 1 ;
         clist[clist[0]] = (k+1)/2-n-nlin;
         if ( k % 2 == 0 )
         {
           gres[0][(k+1)/2-n] = -one ;
         }
         else
         {
           gres[0][(k+1)/2-n] = one ;
         }
      }
    }      
    if ( clist[0] != 0 )
    {
  /*  this evaluates the gradients of the nonlinear constraints which were    */
  /*  active at the old point also at the new point (necessary for the update)*/

       escongrad(clist,nlin,x,gres);
       for ( i = 1 ; i <= clist[0] ; i++ )
       {
         cgres[clist[i]+nlin] += 1 ;   
         val[clist[i]+nlin]    = TRUE ;
         gresn[clist[i]+nlin] = max(one,sqrt(o8sc4(1,n,clist[i]+nlin,gres)));
       }
    }
    o8egph(gphi1);
    
    for (i = 1 ; i <= n ; i++) 
    {
        o8opti_yx[i] = x[i]-x0[i];
        o8opti_yy[i] = gphi1[i]-gphi0[i];
    }
    /* since a represents the Cholesky-factor, this sqrt */
    
    term = sqrt(o8vecn(1,n,o8opti_yy)/o8vecn(1,n,o8opti_yx));
    if ( term != zero && phase >= 0 ) matsc = max(one/scfmax,min(scfmax,term/2));
    
    /* current scaling of identity in case of restart */

    aalist[0] = 0;
    clist[0] = 0 ;
    alist[0] = 0 ;
    /* clist is the list of nonlinear active constraints whose gradients */
    /* need to be evaluated                                              */
    for (i = 1 ; i <= nres ; i++) 
    {
        u0[2*i-1]    = u[2*i-1];
        o8bind0[2*i-1] = o8bind[2*i-1];
        res0[2*i-1]  = res[2*i-1];
        u0[2*i]    = u[2*i];
        o8bind0[2*i] = o8bind[2*i];
        res0[2*i]  = res[2*i];
        if ( low[i] == up[i] ) 
        {
            aalist[0] += 1 ;
            aalist[aalist[0]] = 2*i-1 ;
            o8bind[2*i-1] = 1;
            o8bind[2*i]   = 0;
            if ( i > n+nlin  && ! val[i-n] )
            {
              clist[0] += 1 ;
              clist[clist[0]] = i-n-nlin ;
            }
            if ( i > n )
            {
              alist[0] +=1 ;
              alist[alist[0]] = i-n;
            }
        } 
        else 
        {
            o8bind[2*i-1] = 0;
            o8bind[2*i]   = 0;
            term = res[2*i-1];
            term1 = res[2*i] ;
            if ( i > n ) 
            {
              term /= gresn[i-n];
              term1 /= gresn[i-n];
            }
            else
            {
              term /= max(one,xsc[i]);
              term1 /= max(one,xsc[i]);
            }
            if ( min(term,term1) <= delmin )
            { 
  /* an active inequality constraint at the new point    */
  /* may be the gradient has been evaluated already (for computing the update)*/  
              if ( term <= term1 )
              {
                 o8bind[2*i-1] = 1;
                 aalist[0] += 1;
                 aalist[aalist[0]] = 2*i-1 ;
                 if ( i > n )
                 {
                   alist[0] +=1 ;
                   alist[alist[0]] = i-n ;
                   gres[0][i-n] = one ;
                   if ( i > n+nlin && ! val[i-n] )
                   { 
                     clist[0] += 1;
                     clist[clist[0]] = i-n-nlin ;
                   }
                 }
              }
              else
              {
                 o8bind[2*i] = 1 ;
                 aalist[0] +=1 ;
                 aalist[aalist[0]] = 2*i ;
                 if ( i > n )
                 {
                   alist[0] +=1 ;
                   alist[alist[0]] = i-n ;
                   gres[0][i-n] = -one ;
                   if ( i > n+nlin && ! val[i-n] )
                   {
                     clist[0] += 1;
                     clist[clist[0]] = i-n-nlin ;
                   }
                 }
              }
            } /* end an active constraint */ 
        } /* end inequality case */     
    } /* end for i */

    if ( clist[0] != 0 )
    {
  /*  this evaluates the gradients of the nonlinear constraints which were    */
  /*  active at the new point but not at the old point                        */
       
       escongrad(clist,nlin,x,gres);
       for ( i = 1 ; i <= clist[0] ; i++ )
       {
         cgres[clist[i]+nlin] += 1 ;   
         val[clist[i]+nlin]    = TRUE ;
         gresn[clist[i]+nlin] = max(one,sqrt(o8sc4(1,n,clist[i]+nlin,gres)));
       }
    }

    /* **** o8bind now corresponds to the state of the new point               */
    /* **** but there may be gradients evaluated at the new point            */
    /* **** not yet put to bind                                              */
    /* ****                                                                  */
    /* **** update the unprojected quasi-Newton-matrix anyway                */
    /* ****                                                                  */

    if ( scf != zero ) 
    {
        if ( csirup > nreset || csssig > nreset || csreg  > nreset ) 
        {
            csreg  = 0;
            csssig = 0;
            csirup = 0;
         
            o8inim();
            
        } 
        else 
        {
        
            o8bfgs();
         
            /* for projected update:  if ( ! silent ) o8info(13) */
            /* for Pantoja&Mayne update:                         */
        
            if ( ! silent ) o8info(14);
        }
    }
    /* proceed */

    if ( accinf[itstep][27] == one ) 
    {
        if ( itstep > 1 && 
            accinf[itstep-1][29] != zero && accinf[itstep][29] != zero ) 
        {

            /* c ount s uccessive ir regular up dates */
            
            csirup = csirup+1;
        } 
        else 
        {
            csirup = 0;
        } 
    }
    /* accinf[itstep][27] = 1   update Pantoja&Mayne                   */
    /*                    = 0   noupdate                               */
    /*                    = -1  restart                                */
    /*                    = 2   normal BFGS (nr = 0)                   */
    /*                    = 3   Powell's modified BFGS (nr = 0)        */
    /* accinf[itstep][28] = modification term tk/ den2/den1  resp.     */
    /* accinf[itstep][29] = modification factor th/xsik resp.          */
    /* csirup counts the number of successive irregular updating steps */
    
    if ( phase == -1 ) 
    {
    
        goto L100;
        
    }
    else
    {
    
        goto L200;
    }
    L400:

    singul = TRUE;
    phase  = min(phase,0);
    accinf[itstep][10] = one;
    
    /* try to compute a descent direction using                       */
    /* an extended quadratic program with                             */
    /* individual slack variable for any constraint                   */
    /* compute damping factor for tangential component if upsi>tau0/2 */
    /* by rescaling f if possible                                     */
    
    scf0 = one;
    if ( phase >= 0 && upsi > tau0*p5 ) 
    {
        scfh = max(one/scfmax,
               min(scfmax,(two*(tau0-upsi)/tau0)*upsi*tau/max(one,gfn) ) );
        if ( (fxst-fx)*scfh+scfh/scf*(psist-psi) >= 
             scfh/scf*eta*clow && lastch <= itstep-4
             && (scfh < tm1*scf || scfh > tp1*scf) )
        {
             
            /* rescale the objective function if this seems promising and */
            /* the change is significant                                  */
            
            clow  = clow+1;
            term  = scfh/scf;
            scf0  = term ;
            psi   = psi*term;
            psist = psist*term;
            for (i = 1 ; i <= 2*nres ; i++) 
            {
                u[i] = u[i]*term;
            }
            unorm  = unorm*term;
            scf    = scfh;
            lastch = itstep;
            accinf[itstep][15] = scf;
            term = sqrt(term);
            for ( i = 1 ; i <= n ; i++) 
            {
                diag0[i] = term*diag0[i];
                for ( j = 1 ; j <= n ; j++) 
                {
                    a[j][i] = a[j][i]*term;
                }
            }
            matsc = matsc*term;
            if ( ! silent ) o8msg(2);
        }
    }
    /* slack is upsi at most */
    
    accinf[itstep][32] = upsi;
    if ( ! silent ) o8info(15);

    accinf[itstep][13] = -one;
    term  = fabs(a[1][1]);
    term1 = term;
    i     = 2;
    while ( i <= n ) 
    {
        term  = max(term ,fabs(a[i][i]));
        term1 = min(term1,fabs(a[i][i]));
        i     = i+1;
    }
    accinf[itstep][14] = pow(term/term1,2);
    if ( ! silent ) o8info(5);
    clwold = clow;
    
    /* save for restart */
    
    tauqp0 = tauqp;
    for (i = 1 ; i <= 2*nres ; i++) 
    {
        u[i] = zero;
    }
    /* no second order correction here */
    for (i = 1 ; i <= n ; i++) 
    {
        dd[i] = zero;
    }
    /* solve full qp */
    o8qpdu();
    
    if ( o8dnorm == zero && qpterm == 1 && optite == three ) return;
    if ( o8dnorm <= epsx*(min(xnorm,one)+epsx) && qpterm < 0 ) 
    {   /* QP solver indicated trouble */
    
        /* may be it failed because of illconditioning */
    
        if ( upsi >= nres*delmin && qpnew ) 
        {
        
            /* restarting the method has been done already: game is over */
            
            optite = -one;
            if ( ! silent ) o8info(18);
            
            return;
        }
        if ( qpnew ) 
        {
            optite = qpterm-five;
            if ( ! silent ) o8info(18);
            
            return;
        }
        /* try a = id */
        
        qpnew = TRUE;
        for (i = 1 ; i <= 2*nres ; i++) 
        {
            w[i] = one;
        }
        lastch = itstep;
        
        o8inim();
            
        ident = TRUE;
        
        /* formerly tauqp = tauqp0 */
        
        tauqp    = one;
        aalist[0] = nrbas;
        for (i = 1 ; i <= 2*nres ; i++) 
        {
            o8bind[i] = o8opti_bindba[i];
        }
        if ( scf == zero ) 
        {
        
            goto L100;
            
        } 
        else 
        {
        
            goto L200;
        }
    }
    accinf[itstep][11] = zero;
    o8opti_delist[0] = 0;
    umin      = zero;
    
    /* b2n is defined also internally in o8qpdu */
    
    o8shms();
    
    if ( (qpterm >= 0 || qpterm == -3) && scf != zero ) 
    {
        unorm = fabs(u[1]);
        for (i = 2 ; i <= 2*nres ; i++) 
        {
            unorm = max(unorm,fabs(u[i]));
        }
        for (i = 1 ; i <= n ; i++)
        {
            o8opti_yx[i] = scf*gradf[i];
            for (j = 1 ; j <= nlin+nonlin ; j++)
            {
                o8opti_yx[i] -= gres[i][j]*(u[2*j-1+2*n]-u[2*j+2*n]);
            }
        }
        for (i = 1 ; i <= n ; i++ )
        {
            o8opti_yx[i] -= xsc[i]*(u[2*i-1]-u[2*i]) ;
        }
        b2n = o8vecn(1,n,o8opti_yx)/scf;
    } 
    else 
    {
        b2n = -one;
        
        /* signals "undefined" here */
    }
    if ( ! silent ) o8info(2);
    if ( ! silent ) o8info(16);
    
    /* to avoid termination */
    
    if ( b2n == -one ) b2n = epsmac/tolmac;
    if ( qpterm >= 0 && o8dnorm <= tm2*epsx*(epsx+min(one,xnorm))) 
    {
        if ( upsi <= nres*delmin ) 
        {
            optite = six;
        }
        else 
        {
            optite = -five;
        }
        return;
    }
    /* check whether QPsolver terminated unsuccessfully */
    
    if ( qpterm < 0 ) 
    {
    
        /* we have a unfeasible solution for QP. try it */
        /* but don't complain if it fails               */
        
        if ( ! silent ) o8msg(10);
    }
    if ( clow > clwold ) 
    {
        term = one;
        for (i = 1 ; i <= 2*nres ; i++) 
        {
            term = max(term,w[i]);
        }
        tauqp = max(one,term);
    }
    if ( tauqp > pow(taufac,3)*tauqp0 ) tauqp = pow(taufac,3)*tauqp0;
    
    /* no change of tauqp otherwise */
    
    b2n0   = b2n;
    umin   = zero;
    uminsc = zero;
    if ( qpterm >= 1 ) 
    {
      slackn = zero;
      for (i = 1 ; i <= nres ; i++)  
      { 
        term = res[2*i-1];
        term1 = res[2*i];
        denom = one ;
        if ( i > n ) denom = gresn[i-n];
        slackn +=
            max(zero,term/denom-delmin)*max(zero,u[2*i-1]-smallw)/denom
           +max(zero,term1/denom-delmin)*max(zero,u[2*i]-smallw)/denom;
      }

    } 
    else 
    {
    
        /* slack is undefined, since multipliers are undefined */
        /* use this value to prevent premature termination     */
        
        slackn = one;
    }
    goto L350;
}

void o8inim(void) 
{
/* ************************************************************************ */
/*      initialize the quasi Newton update with a multiple of the identity  */
/* ************************************************************************ */


    void o8info(int icase);
    
    static int i,j;

    for (i = 1 ; i <= n ; i++) 
    {
        for (j = 1 ; j <= n ; j++) 
        {
            a[j][i] = zero;
        }
        a[i][i]  = matsc;
        diag0[i] = matsc;
    }
    accinf[itstep][27] = -one;
    accinf[itstep][14] = one;
    if ( ! silent ) o8info(20);
    
    return;
}
void o8dird(void)
{
/* **************************************************************************** */
/*          compute the directional derivative of the L1-penalty-function       */
/* **************************************************************************** */
    
    double o8sc1(int i,int j,double a[],double b[]);
    double o8sc3(int n,int m,int j,double **a,double b[]);
    
    static int  i;
    static double   term/*,term1*/;

    /* compute directional derivative of Zangwill function */
    
    /* L100: */

    dirder = o8sc1(1,n,gradf,d)*scf;
    
    for (i = 1 ; i <= nres ; i++) 
    {
       if ( i <= n )
       {
       /*  bound constraints                                    */
          if ( low[i] == up[i] ) 
          /*    equality constraint type h(x)=x(i)-low(i)=0     */
          {
            if ( res[2*i-1] <= -tp3*epsmac )
            {
               dirder -=  d[i]*w[2*i-1];
            }
            else if ( res[2*i-1] >= tp3*epsmac )
            {
               dirder += d[i]*w[2*i-1];
            }
            else
            {
               dirder += fabs(d[i])*w[2*i-1];
            }
          }
          else
          {
            if ( o8bind[2*i-1] == 1 )
            /*  active lower bound */
            {
              term = w[2*i-1]*d[i] ;
              if ( fabs(res[2*i-1]) <= tp3*epsmac )
              {
                dirder -=  min(zero,term);
              }
              else
              { 
                if ( res[2*i-1] < -tp3*epsmac )
                {
                  term = min ( term , -res[2*i-1]*w[2*i-1] );
            /* penalty function can decrease at most by  -res[2*i-1]*w[2*i-1] */
                  dirder -= term ;
                }
              }
            }  
            if ( o8bind[2*i] == 1 ) 
            {
              /* active upper bound */
              term = -d[i]*w[2*i];
              if ( fabs(res[2*i]) <= tp3*epsmac )
              {
                dirder -= min(zero,term);
              }
              else
              {   
                if ( res[2*i] < -tp3*epsmac )
                {
                  term = min ( term , -res[2*i]*w[2*i] );
                  dirder -= term ;
                }
              }  
            }
          }
       }
       else
       {      
          term  = o8sc3(1,n,i-n,gres,d)*gres[0][i-n];
          if ( low[i] == up[i] ) 
          {
            if ( res[2*i-1] <= -tp3*epsmac )
            {
               dirder -=  term*w[2*i-1];
            }
            else if ( res[2*i-1] >= tp3*epsmac )
            {
               dirder += term*w[2*i-1];
            }
            else
            {
               dirder += fabs(term)*w[2*i-1];
            }
          }
          else
          {
            if ( o8bind[2*i-1] == 1 )
            {
              term = w[2*i-1]*term ;
              if ( fabs(res[2*i-1]) <= tp3*epsmac )
              {
                dirder -= min(zero,term);
              }
              else
              { 
                if ( res[2*i-1] < -tp3*epsmac )
                {
                  term = min ( term , -res[2*i-1]*w[2*i-1] );
                  dirder -= term ;
                }
              }
            }  
            if ( o8bind[2*i] == 1 ) 
            {
              term = term*w[2*i];
              if ( fabs(res[2*i]) <= tp3*epsmac )
              {
                dirder -= min(zero,term);
              }
              else
              {   
                if ( res[2*i] < -tp3*epsmac )
                {
                  term = min ( term , -res[2*i]*w[2*i] );
                  dirder -= term ;
                }
              }  
            }
          }
       }
    }
    return;
}

/* **************************************************************************** */
/*                      cut d if appropriate and rescale                        */
/* **************************************************************************** */
void o8cutd(void) 
{

    double o8sc1 (int i,int j,double a[],double b[]);
    double o8vecn(int nl,int nm,double x[]);

    static int  i;
    static double   term,term1;

    xnorm  = o8vecn(1,n,x);
    term   = beta*(xnorm+one);
    o8dnorm  = o8vecn(1,n,d);
    d0norm = o8vecn(1,n,d0);
    dscal  = one;
    if ( o8dnorm*d0norm != zero ) 
    {
        cosphi = o8sc1(1,n,d,d0)/(d0norm*o8dnorm);
    } 
    else 
    {
        cosphi = zero;
    }
    if ( o8dnorm > term ) 
    {
    
        /* d too long: rescale */
        
        term1 = term/o8dnorm;
        o8dnorm = term;
        dscal = term1;
        for (i = 1 ; i <= n ; i++) 
        {
            d[i]  = d[i]*term1;
            dd[i] = dd[i]*pow(term1,2);
        }
    }
    /* since we project the ray with respect to the bounds, be sure */
    /* to compute the directional derivative correctly              */
    /* therefore correct d and dd appropriately                     */
    
    for (i = 1 ; i <= n ; i++) 
    {
        if ( llow[i] && x[i]+sigsm*d[i] <= ug[i] ) 
        {
            d[i]  = zero;
            dd[i] = max(zero,dd[i]);
        }
        if ( lup[i] && x[i]+sigsm*d[i] >= og[i] ) 
        {
            d[i]  = zero;
            dd[i] = min(zero,dd[i]);
        }
    }
    o8dnorm = o8vecn(1,n,d);
    
    return;
}

/* **************************************************************************** */
/* compute maximum stepsize stmaxl such that projection on the box of lower     */
/* and upper bounds changes for sig in [0,stmaxl],if such exists                */
/* **************************************************************************** */
void o8smax(void) {
    
    static int  i;
    static int  exis;
    /* exis is true if there exists some sigmaxl, such that the projection */
    /* of x+sigma*d on the feasible box does not change for sigma>=sigmaxl */
    /* that means no coordinate changes any more                           */ 
    exis = TRUE;

    for (i = 1 ; i <= n ; i++) {
        exis = exis &&( ( d[i] == zero )
            || ( lup[i]  && d[i] > zero )
            || ( llow[i] && d[i] < zero ) );
    }
    if ( exis ) 
    {
        stmaxl = sigsm;
        for (i = 1 ; i <= n ; i++) {
            if ( llow[i] && d[i] < zero ) {
                if ( -d[i]*sigla >= x[i]-ug[i] ) {
                    stmaxl = max(stmaxl,(x[i]-ug[i])/(-d[i]));
                } else {
                    stmaxl = sigla;
                }
            }
            if ( lup[i] && d[i] > zero ) {
                if ( d[i]*sigla >= og[i]-x[i] ) {
                    stmaxl = max(stmaxl,(og[i]-x[i])/d[i]);
                } else {
                    stmaxl = sigla;
                }
            }
        }
    } else {
        stmaxl = sigla;
    }
    /* but never use stepsize larger than sigla */
    
    stmaxl = min(sigla,stmaxl);
    
    return;
}

/* **************************************************************************** */
/*        restore the best point found so far to be the current new point       */
/* **************************************************************************** */
void o8rest(void) {
    
    static int  j;

    phi1  = phimin;
    psi1  = psimin;
    upsi1 = upsim;
    sig   = sigmin;
    fx1   = o8fmin;
    for (j = 1 ; j <= n ; j++) {
        x1[j] = xmin[j];
    }
    for (j = 1 ; j <= 2*nres ; j++) {
        res1[j] = resmin[j];
    }
    return;
}

/* **************************************************************************** */
/*             save the best point found so far in ...min variables             */
/* **************************************************************************** */
void o8save(void) {
    
    static int  i;
    
    phimin = phi1;
    upsim  = upsi1;
    psimin = psi1;
    o8fmin   = fx1;
    sigmin = sig;
    for (i = 1 ; i <= n ; i++) {
        xmin[i] = x1[i];
    }
    for (i = 1 ; i <= 2*nres ; i++) {
        resmin[i] = res1[i];
    }
    return;
}

/* **************************************************************************** */
/*                    evaluate the functions at the new point                   */
/* **************************************************************************** */
void o8eval(double sigact,double *sigres,int *reject,int *o8error) 
{

    void    esf(double x[],double *fx);
    void    escon(int i,int liste[] , double x[],
                                 double con[],int err[]);
    double  o8sc3(int i, int j , int k , 
        double **mat, double x[]);
    void    user_eval(double xvar[],int mode);
    
    static int  i/*,j*/;
    static double   term , denom ;
    /* liste as a formal placeholder, not used here since type of call to */
    /* escon is 1                                                         */
    static int   liste[2] ;
    static int eval_err ;   
    
     
    liste[0] = 0 ;
    liste[1] = 0 ; 
    sig = sigact;
    for (i = 1 ; i <= n ; i ++) 
    {
        x1[i] = x[i]+sig*(d[i]+sig*dd[i]);
        
        /* project with respect to the box-constraints */
        
        x1[i] = max(x1[i],ug[i]);
        x1[i] = min(x1[i],og[i]);
    }
    *reject = FALSE;
    *o8error  = FALSE;
    *sigres = sig;
    upsi1   = zero;
    psi1    = zero;

/**********                                            *******************/
    if ( bloc ) 
    {   
      valid = FALSE ;
      /* user_eval must reset valid */
      user_eval(x1,-1);
    }
/********** only function values, from xtr = x1*xsc    *******************/
    
/********** compute res1 = restrictions at x1     *******************/
    for ( i = 1 ; i <= n ; i++ ) 
    {
       res1[2*i-1] = x1[i]-ug[i];
       res1[2*i  ] = og[i]-x1[i];
    }
    for ( i = 1 ; i <= nlin ; i++ )
    {
       term = o8sc3(1,n,i,gres,x1);
       cres[i] += 1 ;
       res1[2*(i+n)-1] = term - low[n+i];
       res1[2*(i+n)  ] = up[n+i]-term;
    }
    for ( i = 1 ; i <= nonlin ; i++ )
    {
      confuerr[i] = FALSE ;
    }
    escon(1,liste,x1,o8eval_con,confuerr);
    /* for call type 1, liste is dummy */
    eval_err = FALSE ;
    for (i = 1 ; i <= nonlin ; i++ )
    {
       cres[nlin+i] += 1 ;
       eval_err = eval_err || confuerr[i] ;
    }
    if ( eval_err ) 
    {
      *o8error = TRUE ;
      return ;
    }
    for ( i = 1 ; i <= nonlin ; i++ )
    {
       res1[2*(n+nlin+i)-1] = o8eval_con[i]-low[i+n+nlin];
       res1[2*(n+nlin+i)  ] = up[i+n+nlin]-o8eval_con[i];
    }

    for ( i = 1 ; i <= nres ; i++ )
    {
      if ( low[i] == up[i] ) 
      {
         term = fabs(res1[2*i-1]);
         upsi1 = upsi1+term;
         if ( upsi1 > tau0 && phase != -1 ) 
         {
            *reject = TRUE;
            return;
         }
 
         psi1 = psi1+term*w[2*i-1];
         denom = ( i<=n ) ? max(one,xsc[i]) : gresn[i-n];
         /*  only the odd numbered constraint is used for equalities */
         if ( res1[2*i-1]*res[2*i-1] < zero && sig <= one && 
              (fabs(res[2*i-1])/denom >= tp3*epsmac 
               || fabs(res1[2*i-1])/denom >= tp3*epsmac ) ) 
            *sigres = min(*sigres,sig*res[2*i-1]/(res[2*i-1]-res1[2*i-1]));
      }      
      else
      {
         term = -min(zero,res1[2*i-1]);
         if ( res1[2*i-1] < -delmin && o8bind[2*i-1] == 0 )
         {
           violis[0] +=1 ;
           violis[violis[0]] = 2*i-1 ;
         }

         upsi1 = upsi1+term;
         if ( upsi1 > tau0 && phase != -1 )
         {
            *reject = TRUE;
            return;
         }
         psi1 = psi1+term*w[2*i-1];
         denom = ( i <= n) ? max(one, xsc[i]) : gresn[i-n];
         if ( res1[2*i-1]*res[2*i-1] < zero && sig <= one && 
              ( o8bind[2*i-1] == 0 ||
              (o8bind[2*i-1] == 1 && (fabs(res[2*i-1])/denom >= tp3*epsmac
              || fabs(res1[2*i-1])/denom >= tp3*epsmac ) ) ) )
            *sigres = min(*sigres,sig*res[2*i-1]/(res[2*i-1]-res1[2*i-1]));

         term = -min(zero,res1[2*i]);
         upsi1 = upsi1+term;
         if ( res1[2*i] < -delmin && o8bind[2*i] == 0 )
         {
           violis[0] += 1;
           violis[violis[0]] = 2*i ;
         }

         if ( upsi1 > tau0 && phase != -1 )
         {
            *reject = TRUE;
            return;
         }
         psi1 = psi1+term*w[2*i];

         if ( res1[2*i]*res[2*i] < zero && sig <= one && ( o8bind[2*i] == 0 ||
            (o8bind[2*i] == 1 && (fabs(res[2*i])/denom >= tp3*epsmac
            || fabs(res1[2*i])/denom >= tp3*epsmac ) ) ) )
            *sigres = min(*sigres,sig*res[2*i]/(res[2*i]-res1[2*i]));

      }
    }
       /* violis is the list of constraints                            */
       /* not binding which have been hit during unidimensional search */



    if ( phase != -1 ) 
    {
        ffuerr = FALSE;
        
        esf(x1,&fx1);
        icf +=  1 ;
        if ( ffuerr ) 
        {
            *o8error = TRUE;
            
            return;
        }
    } 
    else 
    {
        fx1 = zero;
    }
    phi1 = scf*fx1+psi1;
    
    return;
}

/* **************************************************************************** */
/*          determination of stepsize by an Armijo-like test for descent        */
/* **************************************************************************** */
void o8unim(double sig1th) {

    /* sig1th the first proposed stepsize for searching on the arc              */
    /* if sig = one did'nt work                                                 */

    /* n = number of variables                                                  */
    /* x = current point                                                        */
    /* d = direction of descent. dd = second order correction                   */
    /* x,d = input                                                              */
    /* x0,d0 etc. information from previous step                                */

    /* xnorm,dnorm = euclidean length of x and d                                */
    /* stptrm = 1 on success , = -1 or = -2 otherwise                           */
    /* sig    =  computed stepsize                                              */
    /* it is assumed that one is asymptotically optimal                         */
    /* sigsm = smallest acceptable stepsize                                     */
    /* sigla = largest  acceptable stepsize                                     */
    /* alpha = smallest feasible reduction factor for stepsize                  */
    /* delta = multiplier for derivative should be smaller than .25             */
    /* beta  = maximum feasible increase of x-norm for sig = 1                  */
    /* theta = bound for cos(angle(current direction, previous direction))      */
    /*         if overridden, stepsize larger than one is tried                 */

    /* ************************************************************************ */

    void    o8info(int icase);
    void    o8rest(void);
    void    o8save(void);
    void    o8eval(double sigact,double *sigres,int *reject,int *o8error);

    static int  i,l,j;
    static double   term,maxphi;
    static double   sigres ,diff;
    static int  desc,descre,sminfe,lainfe,reject,o8error;
    

    o8unim_step[1] = p5;
    o8unim_step[2] = twom2;
    for (i = 3 ; i <= nstep ; i++) {
        o8unim_step[i] = tm1;
    }
    /* projection of d, rescaling and computing dirder has been done already */
    
    l         = 0;
    o8error     = FALSE;
    phi       = scf*fx+psi;
    sig       = sig1th;
    violis[0] = 0;
    if ( ! silent ) o8info(8);
    
    L100:

    l = l+1;
    if ( l > nstep ) {
        if ( o8error && ! silent ) o8msg(24);
        stptrm = -one;
        sig    = zero;
        
        return;
    }
    /* compute a new x and test for descent */
    
    o8eval(sig,&sigres,&reject,&o8error);
    
    if ( o8error ) {
        if ( sig > one ) {
        
            o8rest();
            
            goto L200;
            
        } else {
            if ( ! silent ) o8msg(25);
            sig = o8unim_step[l]*sig;
            
            goto L100;
        }
    }
    if ( reject ) {
        if ( sig > one ) {
        
            o8rest();
            
            goto L200;
            
        } else {
            sig = o8unim_step[l]*sig;
            
            goto L100;
        }
    }
    if ( ! silent ) o8info(9);
    
    /* new function value */
    
    if ( sig > one ) {
        if ( phi1 >= phimin ) {
        
            /* phi does'nt decrease further */
            
            o8rest();
            
            goto L200;
            
        } else {
            if ( sig < stmaxl ) {
            
                o8save();
                
                sig = min(stmaxl,sig+sig);
                
                goto L100;
                
            } else {
            
                goto L200;
            }
        }
    }
    if ( lastch >= itstep-3 || phase != 2 || singul ) {
    
        /* require monotonic behaviour */
    
        diff = phi-phi1;
    } else {
        maxphi = phi;
        for (j = 1 ; j <= 3 ; j++) {
            maxphi = max(scf*accinf[itstep-j][2]+accinf[itstep-j][4],maxphi);
        }
        diff = maxphi-phi1;
    }
    desc   = diff >= min(-sig*delta*dirder,level);
    descre = upsi - upsi1 >= sig*pow(delta,2)*upsi/tauqp;
    sminfe = upsi <= tau0*p5 && upsi1 <= tau0;
    lainfe = upsi > tau0*p5;
    
    if ( desc && ( sminfe || ( lainfe && descre ) ) ) {
    
        /* Goldstein-Armijo descent test satisfied */
        
        if ( sig == one && ( (cosphi >= theta && sig0 >= one 
             && (phase+1)*(phase-2) != 0
             && ! singul) || diff >= -sig*delta1*dirder )
             && stmaxl > one && upsi < tau0*p5 ) {
             
            /* 1 >= delta1 >> delta > 0 */
             
            /* try stepsize larger than one */
            /* save the current point as the best one */
            
            o8save();
            
            sig = min(stmaxl,sig+sig);
            
            goto L100;
        }
        if ( sig <= one && upsi > tau0*p5 && upsi1 > upsi ) goto L300;
        
        goto L200;
        
    } else {
    
        goto L300;
    }
    L200:

    /* accept new x, save old values */
    
    fx0    = fx;
    fx     = fx1;
    upsi0  = upsi;
    upsi   = upsi1;
    psi0   = psi;
    psi    = psi1;
    stptrm = one;
    sig0   = sig;
    for (i = 1 ; i <= n ; i++) {
        x0[i]   = x[i];
        d0[i]   = d[i];
        x[i]    = x1[i];
        difx[i] = x[i]-x0[i];
    }
    d0norm = o8dnorm;
    x0norm = xnorm;
    for (i = 1 ; i <= 2*nres ; i++) {
        res[i] = res1[i];
    }
    return;
    
    /* continue reducing sig */
    
    L300:

    if ( sigres < sig ) {
        sig = min(p5*sig,max(o8unim_step[l]*sig,sigres));
    } else {
        term = (diff-dirder*sig)*two;
        if ( term > epsmac*(scf*fabs(fx)+psi) ) {
            sig = min(p5*sig,max(o8unim_step[l]*sig,-dirder*pow(sig,2)/term));
        } else {
            sig = o8unim_step[l]*sig;
        }
    }

    
    if ( sig*max(one,o8dnorm) >= sigsm ) goto L100;
    stptrm = -one;
    sig    = zero;
    
    return;
}

/* **************************************************************************** */
/*                scalar product of two vectors or parts of vectors             */
/* **************************************************************************** */
double o8sc1(int i,int j,double a[],double b[]) 
{

    /* multiply two vectors */
    
    static int  k;
    static double   s;

    if ( i > j ) 
    {
        return (zero);
    } 
    else 
    {
        s = zero;
        for (k = i ; k <= j ; k++) 
        {
            s += a[k]*b[k];
        }
        return (s);
    }
}

/* **************************************************************************** */
/*                    multiply row j of matrix a with vector b                  */
/* **************************************************************************** */
double o8sc2(int n,int m,int j,double **a,double b[]) {
    
    static double   s;
    static int  i;

    s = zero;
    for (i = n ; i <= m ; i++) 
    {
        s = s+a[j][i]*b[i];
    }
    return (s);
}

/* **************************************************************************** */
/*          multiply column j section (n to m) of matrix a with vector b        */
/* **************************************************************************** */
double o8sc3(int n,int m,int j,double **a,double b[]) 
{

    static double   s;
    static int  i;

    s = zero;
    for (i = n ; i <= m ; i++) {
        s = s+a[i][j]*b[i];
    }
    return (s);
}
/*************************************************************************/
double o8sc3b(int n,int m,int j,double **a,double b[])
{
 
    static double   s;
    static int  i;
 
    s = zero;
    for (i = n ; i <= m ; i++) {
        s = s+a[i][j]*b[i];
    }
    return (s);
}

double o8sc4(int n,int m,int j,double **a) 
{

    static double   s;
    static int  i;

    s = zero;
    for (i = n ; i <= m ; i++) {
        s += pow(a[i][j],2);
    }
    return (s);
}

double o8sc3_(int n,int m,int j,double **a,double b[]) {

    static double   s;
    static int  i;

    s = zero;
    for (i = n ; i <= m ; i++) {
        s = s+a[i][j]*b[i];
    }
    return (s);
}

/* **************************************************************************** */
/* subprogram for structured output of a submatrix a[ma][na]                    */
/* on channel lognum in fix or float format with heading "head".                */
/* uses a fixed format string with 70 print columns                             */
/* **************************************************************************** */
void o8mdru(double **a,int ma,int na,char head[],
            FILE *lognum,int fix) {

    static int  i,j,jo,ju;
    static int  anz;

    if ( fsilent ) return;
    
    fprintf(lognum,"\n%40s\n", head);
    anz = 4;
    jo  = 0;
    while ( jo < na ) {
        ju = jo+1;
        jo = min(ju+anz-1,na);
        
        fprintf(lognum,"\nrow/column");
        for (j = ju ; j <= jo ; j++) {
            fprintf(lognum,"      %3i      ", j);
            if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
        }
        for (i = 1 ; i <= ma ; i++) {
            if ( fix ) {
                fprintf(lognum,"   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
                    fprintf(lognum,"%14.7f ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
                }
            } else {
                fprintf(lognum,"   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
                    fprintf(lognum," %13.6e ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
                }
            }
        }
    }
    return;
}
void o8mdru_(double **a,int ma,int na,char head[],
             FILE *lognum,int fix) {

    static int  i,j,jo,ju;
    static int  anz;

    if (!fsilent)
      fprintf(lognum,"\n%40s\n", head);
    if (intakt)
      Rprintf("\n%40s\n", head);
    
    anz = 4;
    jo  = 0;
    while ( jo < na ) {
        ju = jo+1;
        jo = min(ju+anz-1,na);

    if (!fsilent)
      fprintf(lognum,"\nrow/column");
    if (intakt)
      Rprintf("\nrow/column");
    
        for (j = ju ; j <= jo ; j++) {
      if (!fsilent){
            fprintf(lognum,"      %3i      ", j);
            if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
      }
      if (intakt){
            Rprintf("      %3i      ", j);
            if ( (j-ju+1) % 4 == 0 || j == jo ) Rprintf("\n");
      }
        }
        for (i = 1 ; i <= ma ; i++) {
            if ( fix ) {
          if (!fsilent){
                fprintf(lognum,"   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
                    fprintf(lognum,"%14.7f ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
                }
          }
          if (intakt){
                Rprintf("   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
            Rprintf("%14.7f ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) Rprintf("\n");
                }
          }

            } else {

          if (!fsilent){
                fprintf(lognum,"   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
                    fprintf(lognum," %13.6e ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) fprintf(lognum,"\n");
                }
          }
          if (intakt){
                Rprintf("   %4i   ",i);
                for (j = ju ; j <= jo ; j++) {
                    Rprintf(" %13.6e ", a[i][j]);
                    if ( (j-ju+1) % 4 == 0 || j == jo ) Rprintf("\n");
                }
          }

            }
        }
    }
    return;
}

/* **************************************************************************** */
/*                        compute gradient of lagrangian                        */
/* **************************************************************************** */
void o8egph(double gphi[]) {

    static int  i,j,l,k;

    for (i = 1 ; i <= n ; i++) 
    {
         gphi[i] = gradf[i] * scf;
         for (j = 1 ; j <= aalist[0] ; j++) 
         {
             l = aalist[j];
             k = (l+1)/2;
             if( low[k] == up[k] )
             { 
               if ( k > n )
               {
                 gphi[i] -= gres[i][k-n]*u[l] ;
               }
               else
               {
                 gphi[i] -= xsc[k]*u[l]; 
                 /* since an equality constraint is always odd numbered */
               }
             }
             else
             {
                if( u[l] > zero ) 
                {
                  if ( k > n ) 
                  { 
                    gphi[i] = gphi[i]-gres[i][k-n]*gres[0][k-n]*u[l];
                  }
                  else
                  {
                    if ( l%2 == 0 )
                    {
                       gphi[i] += xsc[k]*u[l];
                    }
                    else
                    {
                       gphi[i] -= xsc[k]*u[l];
                    }
                  }
                }
             }

            /* include constraints, whose multipliers are of correct sign only */
         }
    }
    return;
}

/* detection and elimination of redundant equality constraints             */
/* this is done making them always inactive inequality constraints         */
/* by redefining low and up                                                */
void o8elim(void) {
    double  o8sc3b 
      (int n,int m,int j,double **a,double b[]);
    double  o8vecn(int nl,int nm,double x[]);
    void o8msg(int num);
    static int  n1,n2;  
    static int  i,j,k,/*l,*/i1,icur,ipiv;
    static double   sum,term,dalpha,dbeta,qrii;
    static double   curle;
    
    optite = 0 ; /* indicates success */
    icur = 0 ;
    for ( i=1 ; i<= n+nlin ; i++ )
    {
      /* search for linear equality constraints */
      if ( low[i] == up[i] )
      {
        icur += 1;
        if ( i <= n )
        {
          for ( j = 1 ; j <= n ; j++ )
          {
            o8elim_qri[j] = zero ;
          }
          o8elim_qri[i] = xsc[i]  ;
        }
        else
        {   
          for ( j = 1 ; j <= n ; j++ )
          {
             o8elim_qri[j] = gres[j][i-n] ;
          }
        }  
        colle[icur] = o8vecn(1,n,o8elim_qri);
        colno[icur] = i; /* global constraint number */
        o8elim_col[icur] = icur ; /* local column number */   
        o8elim_rhsscal[icur] = one/colle[icur];
        for ( j = 1 ; j <= n ; j++ )
        {
           qr[j][icur] = o8elim_qri[j]/colle[icur] ;
        }
        colle[icur] = one;
        /* here colno points to the indices of the linear equality constraints */
      }
    }  
    if ( icur == 0 ) return ;
    /* there are no linear equality constraints */
    /* otherwise try to determine the rank of the current qr */
    n1  = 1 ;
    n2  = icur;
    rank = 0 ; 
    for ( i = n1 ; i <= n2 ; i++)
    {
       /* search for pivot column */
       ipiv  = i;
       curle = colle[i];
       for (j = i+1 ; j <= n2 ; j++)
       {
         if ( colle[j] > curle )
         {
                curle = colle[j];
                ipiv = j ;
         }
       }  
       /* interchange columns explicitly, if necessary   */
       if ( ipiv != i )
       {
          j           = o8elim_col[i];
          o8elim_col[i]    = o8elim_col[ipiv];
          o8elim_col[ipiv] = j;
          term        = colle[i];
          colle[i]    = colle[ipiv];
          colle[ipiv] = term;
          for (k = 1 ; k <= n ; k++)
          {
             term        = qr[k][i];
             qr[k][i]    = qr[k][ipiv];
             qr[k][ipiv] = term;
          }
       }   
       sum = zero;
       for (j = i ;  j <= n ;  j++)
       {
         term   = qr[j][i];
         o8elim_qri[j] = term;
         sum    = term*term+sum;
       }
        
       if ( sum <= pow(rho,2) )
       {
         /* set tiny values to zero */
         for (j = i ; j <= n2 ; j++)  
         {
           colle[j] = zero;
           for (k = i ; k <= n ; k++)
           {
              qr[k][j] = zero;
           }
         }  
         rank = i-1;
         break;
       }
       qrii   = o8elim_qri[i];
       dalpha = -sqrt(sum);
       if ( qrii < zero ) dalpha = -dalpha;
       dbeta    = one/(sum-qrii*dalpha);   
       diag[i]  = dalpha;
       betaq[i] = dbeta; 
       o8elim_qri[i]   = qrii-dalpha;
       qr[i][i] = o8elim_qri[i];
       rank     = i;
       i1       = i+1;
       for (j = i1 ; j <= n2 ; j++)
       {
         sum = dbeta*o8sc3b(i,n,j,qr,o8elim_qri);
         for (k = i ; k <= n ; k++)
         {
           qr[k][j] = qr[k][j]-sum*o8elim_qri[k];
         }
         colle[j] = colle[j]-pow(qr[i][j],2);
       }
    }   
    /* test */
    /*Rprintf("berechneter rang = %d \n",rank); */
    /* testende */
    if ( rank == n2 ) return ;
    if ( ! silent ) o8msg(28);
    /* the linear equality constraints are sufficiently independent*/
    /* otherwise, check first for compatibility of the (almost) dependent */
    /*  now
       R= [R11 , R12 ; O  O ]
       in matlab notation, where R11 is rank times rank
       R is stored in diag and above the diagonal in qr
    */
    /* constraints : solve transpose(R11)*y=P*b , */
    for ( i = 1 ; i <= rank ; i++ )
    {
       sum = low[colno[o8elim_col[i]]]*o8elim_rhsscal[o8elim_col[i]];
       /* the permuted right hand side, low[i]=up[i] */
       for ( j = i-1 ; j>=1 ; j-- )
       {
         sum -= qr[j][i]*o8elim_y[j] ;
       }
       o8elim_y[i] = sum/diag[i] ;
    }
    for ( i = rank +1 ; i <= n2 ; i++ )
    {
       sum  = low[colno[o8elim_col[i]]]*o8elim_rhsscal[o8elim_col[i]];
       for ( j = 1 ; j<= rank ; j++ )
       {
         term = qr[j][i]*o8elim_y[j] ;
         sum -= term ;
       }
       if ( fabs(sum) >= two*n*rho )
       {
         optite = 8 ;
         if ( ! silent ) o8msg(27);
         return ;
       }
    }   
        
    for ( i = rank +1 ; i <= n2 ; i++ )
    {
      low[colno[o8elim_col[i]]] = -big ;
      up[colno[o8elim_col[i]]]  =  big ;
    }
    /* inhibit use of dependent but compatible linear equality constraints */
    return;
}


/* **************************************************************************** */
/* QR-decomposition of matrix of gradients of binding constraints               */
/* this set may be expanded using multiple calls to o8dec.                      */
/* No exit on singular r-factor here. Information on                            */
/* the decompostion is stored in betaq and in and below the                     */
/* diagonal of qr. r-factor is stored in diag (diagonal ) and                   */
/* above the diagonal of qr. cscal is the column scaling of the                 */
/* original matrix. Column pivoting is done here and is stored in colno         */
/* **************************************************************************** */
void o8dec(int nlow,int nrl) {
    
    double  o8sc3b (int n,int m,int j,double **a,double b[]);
    void    o8ht  (int id,int incr,int is1,int is2,int m,
                   double **a,double beta[],double b[],double c[]);    
    void    o8left(double **a,double b[],double y[],double *yl,int n);
    double  o8vecn(int nl,int nm,double x[]);
    
    static int  n1,n2;
    static int  i,j,k,l,i1,i2,ipiv;
    static double   sum,term,dalpha,dbeta,qrii,dl,fac;
    static double   curle;


    if ( nlow > nrl ) return;
    if ( nlow == 1 ) rank = 0;
    dl = one/(n+n+n);
    for ( i = nlow ; i <= nrl ; i++) {
        diag[i]  = zero;
        betaq[i] = zero;
        colno[i] = i;
        if ( aalist[i] <= 2*n )
        {
          /* a bound constraint */
          k = (aalist[i]+1)/2;
          /* component x[k]     */
          for ( j = 1 ; j <= n ; j++ )
          {
            o8dec_qri[j] = zero ;
          }
          o8dec_qri[k] = ( aalist[i] % 2 == 0 ) ? -xsc[k] : xsc[k] ;
        }
        else
        {
          k = (aalist[i]-2*n+1)/2 ;
          fac = gres[0][k];
          for (j = 1 ; j <= n ; j++) 
          {
            o8dec_qri[j] = gres[j][k]*fac;
          }
        }
        o8left(a,o8dec_qri,o8dec_qri0,&sum,n);
        
        if ( sum == zero ) {
            cscal[i] = one;
            colle[i] = zero;
            for (j = 1 ; j <= n ; j++) {
                qr[j][i] = zero;
            }
        } else {
            for (j = 1 ; j <= n ; j++) {
                o8dec_qri[j] = o8dec_qri0[perm[j]];
            }
            term     = one/sqrt(max(sum,pow(rho,2)));
            cscal[i] = term;
            if ( nlow > 1 ) {
            
                o8ht(1,0,1,rank,n,qr,betaq,o8dec_qri,o8dec_qri0);
                
                for (j = 1 ; j <= n ; j++) {
                    o8dec_qri[j] = o8dec_qri0[j];
                }
            }
            for (j = 1 ; j <= n ; j++) {
                qr[j][i] = o8dec_qri[j]*term;
            }
            /* colle : length of remaining column squared */
    
            colle[i] = pow(o8vecn(rank+1,n,o8dec_qri)*term,2);
        }
    }
    if ( nlow > 1 && rank < nlow-1 ) {
    
        /* shift zero block to the right */
        
        i1 = nlow-1-rank;
        i2 = nrl-nlow+1;
        for (i = 1 ; i <= min(i1,i2) ; i++) {
            ipiv        = rank+i;
            k           = nrl-i+1;
            term        = betaq[k];
            betaq[k]    = betaq[ipiv];
            betaq[ipiv] = term;
            j           = colno[k];
            colno[k]    = colno[ipiv];
            colno[ipiv] = j;
            term        = colle[k];
            colle[k]    = colle[ipiv];
            colle[ipiv] = term;
            for (j = 1 ; j <= n ; j++) {
                term        = qr[j][k];
                qr[j][k]    = qr[j][ipiv];
                qr[j][ipiv] = term;
             }
         }
    }
    if ( nlow > 1 ) {
        n1 = rank+1;
        n2 = n1+nrl-nlow;
    } else {
        n1 = nlow;
        n2 = nrl;
    }
    for ( i = n1 ; i <= n2 ; i++) {
    
        /* search for pivot column */
        
        ipiv  = i;
        curle = colle[i];
        for (j = i+1 ; j <= n2 ; j++) {
            if ( colle[j] > curle ) {
                curle = colle[j];
            }
        }
        for (j = n2 ; j >= i ; j--) {
            if ( colle[j] >= curle/three ) ipiv = j;
        }
        /* interchange columns explicitly, if necessary   */
        /* make interchanges continuous with respect to x */
        
        if ( ipiv != i ) {
            j           = colno[i];
            colno[i]    = colno[ipiv];
            colno[ipiv] = j;
            term        = colle[i];
            colle[i]    = colle[ipiv];
            colle[ipiv] = term;
            for (k = 1 ; k <= n ; k++) {
                term        = qr[k][i];
                qr[k][i]    = qr[k][ipiv];
                qr[k][ipiv] = term;
            }
        }
        sum = zero;
        for (j = i ;  j <= n ;  j++) {
            term   = qr[j][i];
            o8dec_qri[j] = term;
            sum    = term*term+sum;
        }
        if ( sum <= pow(rho,2) ) {
        
            /* set tiny values to zero */
            
            for (j = i ; j <= n2 ; j++) {
                colle[j] = zero;
                for (k = i ; k <= n ; k++) {
                    qr[k][j] = zero;
                }
            }
            rank = i-1;
            
            return;
        }
        qrii   = o8dec_qri[i];
        dalpha = -sqrt(sum);
        if ( fabs(qrii) <= -dalpha*dl ) {
            term = zero;
            for (j = i+1 ; j <= n ; j++) {
                if ( fabs(o8dec_qri[j]) > term ) {
                    term = fabs(o8dec_qri[j]);
                    l    = j;
                }
            }
            k        = perm1[i];
            perm1[i] = perm1[l];
            perm1[l] = k;
        }
        if ( qrii < zero ) dalpha = -dalpha;
        dbeta    = one/(sum-qrii*dalpha);
        diag[i]  = dalpha;
        betaq[i] = dbeta;
        o8dec_qri[i]   = qrii-dalpha;
        qr[i][i] = o8dec_qri[i];
        rank     = i;
        i1       = i+1;
        for (j = i1 ; j <= n2 ; j++) {
            sum = dbeta*o8sc3b(i,n,j,qr,o8dec_qri);
            for (k = i ; k <= n ; k++) {
                qr[k][j] = qr[k][j]-sum*o8dec_qri[k];
            }
            colle[j] = colle[j]-pow(qr[i][j],2);
        }
    }
    return;
}

/* **************************************************************************** */
/*                  application of Householder transformations                  */
/* **************************************************************************** */
void o8ht(int id,int incr,int is1,int is2,int m,
          double **a,double beta[],double b[],double c[]) {

    /* application of Householder transformations stored     */
    /* in the lower or strict lower (if incr = 0 or 1 resp.) */
    /* triangle of a and in the vector beta on b giving c.   */
    /* only columns is1 to is2 are used in forward manner    */
    /* if id > 0,backwards otherwise.                        */
    /* rows is1 to m of c are changed only                   */
    /* a stands here for the actual qr                       */
    double o8sc3b(int n,int m,int j,double **a,double b[]);

    static double   sum;
    static int  i,j,k,it;

    for (i = 1 ; i <= m ; i++) {
        c[i] = b[i];
    }
    if(is1 > m)   return;
    if(is2 < is1) return;
    for (i = is1 ; i <= is2 ; i++) {
        it = i;
        if(id < 0) it = is2-it+is1;
        
        /* it = index of transformation */
        
        j   = it+incr;
        sum = beta[it]*o8sc3b(j,m,it,a,c);
        for (k = j ; k <= m ; k++) {
            c[k] = c[k]-sum*a[k][it];
        }
    }
    return;
}
    
/* **************************************************************************** */
/* solve triangular system r*x = b, r defined by Householder-QR-                */
/* decomposition decomp (with column scaling)                                   */
/* **************************************************************************** */
void o8sol(int nlow,int nup,double b[],double x[]) {
    
    static double   sum;
    static int  i,j;

    for (i = nup ; i >= nlow ; i--) {
        sum = zero;
        for (j = i+1 ; j <= nup ; j++) {
            sum = sum+qr[i][j]*o8sol_xl[j];
        }
        o8sol_xl[i] = (b[i]-sum)/diag[i];
    }
    for (i = nlow ; i <= nup ; i++) {
        x[i] = o8sol_xl[i]*cscal[colno[i]];
    }
    /* there must follow interchange of x as given by colno */
    /* e.g. xx(colno[i]) = x[i]                             */

    return;
}

/* **************************************************************************** */
/* solve triangular system r(transpose)*x = b, r defined by                     */
/* Householder-QR-decomposition decomp (with column scaling]                    */
/* **************************************************************************** */
void o8solt(int nlow,int nup,double b[],double x[]) {

    static int  i,j;
    static double   sum;

    for (i = nlow ; i <= nup ; i++) {
    
        /* b has been permuted already ! */
        
        x[i] = b[i]*cscal[colno[i]];
    }
    for (i = nlow ; i <= nup ; i++) {
        sum = zero;
        for (j = nlow ; j <= i-1 ; j++) {
            sum = sum+qr[j][i]*x[j];
        }
        x[i] = (x[i]-sum)/diag[i];
    }
    return;
}
    
/* **************************************************************************** */
/* lenght of vector (a,b). numerically stable version with                      */
/* overflow / underflow saveguard                                               */
/* **************************************************************************** */
double o8dsq1(double a,double b) {

    static double   a1,b1;
    static double   res;

    a1 = fabs(a);
    b1 = fabs(b);
    if ( a1 > b1 ) {
        res = a1*sqrt(one+pow(b1/a1,2));
    } else {
        if ( b1 > a1 ) {
            res = b1*sqrt(one+pow(a1/b1,2));
        } else {
            res = a1*sqrt(two);
        }
    }
    return (res);
}

/* **************************************************************************** */
/*                  computes the upper triangular Cholesky-factor               */
/* **************************************************************************** */
void o8upd(double **r,double z[],double y[],int n,int *fail) {
    
    double  o8dsq1(double a,double b);
    void    o8left(double **a,double b[],double y[],double *yl,int n);
    static int  i,j,i1;
    static double   yl,zl,wl,wn1,ai,bi,h;

    /* o8upd computes the upper triangular Cholesky-factor               */
    /* r1 of r(transpose)*r+z*z(transpose)-y*y(transpose)                */
    /* and restores it in r. The strict lower triangle of r              */
    /* remains unchanged.                                                */
    /* fail = TRUE if the decomposition does'nt exist, stop on dimension */
    /* error, fail = false on success.                                   */
    
    *fail = FALSE;
    
    /* save subdiagonal */
    
    for (i = 1 ; i <= n-1 ; i++) {
        o8upd_sdiag[i]  = r[i+1][i];
        r[i+1][i] = zero;
    }
    /* step one: include z*z(transpose) */
    
    zl = zero;
    for (i = 1 ; i <= n ; i++) {
        zl = zl + pow(z[i],2);
    }
    if ( zl != zero ) {
    
        /* solve w(transpose)*r = z(transpose) */
        
        o8left(r,z,o8upd_w,&wl,n);
        
        wl = sqrt(wl+one);
        
        /* u[2]*u[3]*...*u[n]*w = ( norm(w),0,..,0)(transpose) */
        /* u[i] rotations                                      */
        
        for (i = n ; i >= 2 ; i--) {
            if ( o8upd_w[i] != zero ) {
                i1        = i-1;
                ai        = o8upd_w[i1];
                bi        = o8upd_w[i];
                o8upd_w[i1]     = o8dsq1(ai,bi);
                ai        = ai/o8upd_w[i1];
                bi        = -bi/o8upd_w[i1];
                r[i][i1]  = bi*r[i1][i1];
                r[i1][i1] = ai*r[i1][i1];
                for (j = i ; j <= n ; j++) {
                    h        = ai*r[i1][j] - bi*r[i][j];
                    r[i][j]  = bi*r[i1][j] + ai*r[i][j];
                    r[i1][j] = h;
                }
            }
        }
        /* r = d*r, d = diag(wl,1,...,1), r now Hessenberg */
        
        for (i = 1 ; i <= n ; i++) {
            r[1][i] = r[1][i]*wl;
        }
        /* r = u[n-1]*...*u[1]*r now upper triangular, */
        /* u[i] givens-rotations                       */
        
        for (i = 1 ; i <= n-1 ; i++) {
            i1 = i+1;
            ai = r[i][i];
            bi = -r[i1][i];
            h  = o8dsq1(ai,bi);
            if ( h != zero ) {
                ai       = ai/h;
                bi       = bi/h;
                r[i][i]  = h;
                r[i1][i] = zero;
                for (j = i+1 ; j <= n ; j++) {
                    h        = ai*r[i][j] - bi*r[i1][j];
                    r[i1][j] = bi*r[i][j] + ai*r[i1][j];
                    r[i][j]  = h;
                }
            }
        }
    }
    /* step two : include -y*y(transpose) */
    
    yl = zero;
    for (i = 1 ; i <= n ; i++) {
        yl = yl + pow(y[i],2);
    }
    if ( yl != zero ) {
    
        o8left(r,y,o8upd_w,&wl,n);
        
        if ( wl >= one ) {
            *fail = TRUE;
        } else {
            wl  = sqrt(fabs(one-wl));
            wn1 = wl;
            
            /* ************************************************ */
            /*   ( r(new), 0 )                  (    r  , w )   */
            /*   (-----------)  =  u[1]*...u[n]*(-----------)   */
            /*   (y(transp),1)                  ((0,..,0),wl)   */
            /* ************************************************ */
            
            for (i = n ; i >= 1 ; i--) {
                ai  = wn1;
                bi  = o8upd_w[i];
                wn1 = o8dsq1(ai,bi);
                if ( wn1 != zero ) {
                    ai      = ai/wn1;
                    bi      = bi/wn1;
                    o8upd_rn1[i]  = bi*r[i][i];
                    r[i][i] = ai*r[i][i];
                    for (j = i+1 ; j <= n ; j++) {
                        h       = ai*r[i][j] - bi*o8upd_rn1[j];
                        o8upd_rn1[j]  = bi*r[i][j] + ai*o8upd_rn1[j];
                        r[i][j] = h;
                    }
                }
            }
        }
    }
    /* restore subdiagonal */
    
    for ( i = 1 ;  i <= n-1 ;  i++) {
        r[i+1][i] = o8upd_sdiag[i];
    }
    return;
}

/* **************************************************************************** */
/*                                    o8rght                                    */
/* **************************************************************************** */
void o8rght(double **a,double b[],double y[],double *yl,int n) {
    
    static int  i,j;
    static double   h;

    /* o8rght assumes that the Cholesky-factor of a         */
    /* a = r(transpose)*r is stored in the upper half of a. */
    /* b is a right hand side. o8rght solves                */
    /* r*y = b                                              */
    /* yl = pow(norm(y),2)                                  */
    
    *yl = zero;
    for (i = n ; i >= 1 ; i--) {
        h = b[i];
        for (j = i+1 ; j <= n ; j++) {
            h = h - a[i][j]*y[j];
        }
        h    = h/a[i][i];
        y[i] = h;
        *yl  = pow(h,2) + *yl;
    }
    return;
}

/* **************************************************************************** */
/*                                  o8left                                      */
/* **************************************************************************** */
void o8left(double **a,double b[],double y[],double *yl,int n) {
    
    static int  i,j;
    static double   h;

    /* o8left assumes that the Cholesky-factor of a         */
    /* a = r(transpose)*r is stored in the upper half of a. */
    /* b is a right hand side. o8left solves                */
    /* r(transpose)*y = b                                   */
    /* yl = pow(norm(y),2)                                  */
    
    *yl = zero;
    for (i = 1 ; i <= n ; i++) {
        h = b[i];
        for (j = 1 ; j <= i-1 ; j++) {
            h = h - a[j][i]*y[j];
        }
        h    = h/a[i][i];
        y[i] = h;
        *yl  = pow(h,2) + *yl;
    }
    return;
}

/* **************************************************************************** */
/*                      euclidean norm of x , avoid overflow                    */
/* **************************************************************************** */
double o8vecn(int nl,int nm,double x[]) {

    static int  i;
    static double   xm,h;

    if ( nm < nl ) {
    
        return (zero);
    }
    xm = fabs(x[nl]);
    for (i = nl+1 ; i <= nm ; i++) {
        xm = max(xm,fabs(x[i]));
    }
    if ( xm == zero ) {
    
        return (zero);
        
    } else {
        h = zero;
        for (i = nl ; i <= nm ; i++) {
            h = h+pow(x[i]/xm,2);
        }
        return (xm*sqrt(h));
    }
}
    
/* ************************************************************************* */
/*  solution of extended quadratic program                                   */
/*                                                                           */
/*  scf*gradf(x)*dir+(1/2)*dir*a*dir+summe(tauqp*sl[i]+( my/2)*pow(sl[i],2) )*/
/*  minimal subject to                                                       */
/*  sl[i] >= 0, i = 1,...,nr  (the slacks)                                   */
/*  (gres[.][j]*dir+res[j])+vz*sl[j] = 0, j = 1,...,nh vz = -sign(res[j])    */
/*  (gres[.][aalist[j]])*dir+res[aalist[j]])+sl[j] >= 0, j = nh+1,....,nr    */
/*  the weight tauqp is adapted during solution                              */
/*  the quasi-Newton-matrix a is taken from o8comm.h                         */
/*  a is regularized if not sufficiently well conditioned                    */
/*  the resulting dir=xd[1+nr],...,xd[n+nr] is a direction of descent for    */
/*  the Zangwill function of the corresponding nonlinear                     */
/*  optimization problem                                                     */
/*  f(x) = min, res[j] = 0, j = 1,..nh, res[j] >= 0 , j = nh+1,nres          */
/*  at the currrent point x if the weight tauqp is chosen appropriately      */
/*  the quadratic programming problem is solved using the method             */
/*  of Goldfarb and Idnani                                                   */
/*  variables are stored in xd (solution) and ud (multipliers)               */
/*  in the following order xd = ( sl[j], j = 1,nr; dir = direct. of desc.)   */
/*  ud = ( multipliers for sl[i] >= 0 , i = 1,..,nr ;                        */
/*  multipliers for the equality constraints ,                               */
/*  multipliers for the general inequality constraints )                     */
/* ***************************************************************************/
void o8qpdu(void) 
{
    
    void    o8info(int icase);
    void    o8msg (int num);
    void    o8dird(void);
    void    o8cutd(void);
    double  o8sc1 (int i,int j,double a[],double b[]);
    double  o8vecn(int nl,int nm,double x[]);
    void    o8zup (double z[]);
    void    o8rup (double rv[]);
    void    o8dlcd(int ai[],int l);
    void    o8adcd(void);
    void    o8rinv(int n,double **a,int ndual,double **x);
    
    static double   infe1,s1,s2,tiny,
                    my,zz,ss,su,t,t1,t2,f,fmax,psid,c1,c2,cdiag,term,
                    su1,su2,condr,infiny,term1,/*term2,*/
                    diff0;
    static int  i,j,k,ip,l,incr,nosucc;
    static int  wlow;

    ndual = n+nr;  /* nr=aalist[0] */
    
    /* number of equality constraints in QP-problem */
    incr = nr ;
    mi = 0 ;
    me = 0 ; 
    /* compute the index lists of the QP's equality constraints 
       eqlist and inequality constraints iqlist from aalist and
       low and up */
   for ( i = 1; i <= aalist[0] ; i++ )
   {
     l = (aalist[i]+1)/2;
     if ( low[l] == up[l] )
     {
       me += 1 ;
       o8qpdu_eqlist[me] = aalist[i] ;
     }
     else
     {
       mi += 1 ;
       o8qpdu_iqlist[mi+incr] = aalist[i]+nr ;
     }
   }
   for ( i = 1 ; i <= nr ; i++ ) 
   {
       o8qpdu_iqlist[i] = i ;   /* the slacks of the QP model */
   }
   mi += nr ;

   /* QP inequality constraints = active constraints - */
   /* equality-constraints + slack's                   */
    
   infiny = epsmac/tolmac;
   if ( analyt ) 
   {
        tiny = 2*nr*epsmac*tp3;
   } 
   else 
   {
       tiny = 2*nr*max(epsdif,epsmac*tp3);
   }

   qpterm = 0;
   for (i = 1 ; i <= nr ; i++) 
   {
    
        /* check gradients of active constraints against zero */
        o8qpdu_mult [i] = one ;   
        /* for a zero gradient the slack cannot be diminished:*/ 
        /*   set mult[i] = 0                                  */
        l = aalist[i] ;
        if ( l > 2*n ) 
        {  /* not a bound */
           for (j = 1 ; j <= n ; j++) 
           {
             o8qpdu_y[j] = gres[j][(l-2*n+1)/2];
           }
           if ( o8vecn(1,n,o8qpdu_y) == zero ) 
           {
             o8qpdu_mult[i] = zero;
             if ( ! silent ) o8msg(8);
           }
        }
   }  
   /* restart point in case of increase of tauqp */
    
    L10:
    nosucc = 0 ; 
    /* initialize matrices j and r */
    
    for (i = 1 ; i <= ndual ; i++) 
    {
        ddual[i] = zero;
        for (j = 1 ; j <= ndual ; j++) 
        {
            r[j][i]  = zero;
            xj[j][i] = zero;
        }
    }
    rnorm = one;
    rlow  = one;
    term1 = zero;
    for (i = 1 ; i <= 2*nres ; i++) 
    {
        u[i] = zero;
        if ( w[i] > term1 ) term1 = w[i];
    }
    accinf[itstep][19] = clow;
    accinf[itstep][20] = term1;
    accinf[itstep][31] = tauqp;
    for (i = 1 ; i <= 2*nr ; i++) 
    {   /* the multipliers in the QP */
        ud[i] = zero;
    }
    c1 = fabs(a[1][1]);
    for (i = 1 ; i <= n ; i++) 
    {
        c1 = max(c1,fabs(a[i][i]));
    }
    c1 = c1*tp1;
    
    /* we require much more regularity of a in the singular case */
    
    for (i = 1 ; i <= n ; i++) 
    {
        if ( fabs(a[i][i]) < sqrt(rho1)*c1 ) a[i][i] = sqrt(rho1)*c1;
    }
    /* invert the Cholesky-factor and store in 
       the right lower block of dimension n of xj ( Idnanis j-matrix) */
    
    o8rinv(n,a,ndual,xj);
    
    c1   = fabs(a[1][1]);
    incr = nr;
    c2   = fabs(xj[1+incr][1+incr]);
    for (i = 1 ; i <= n ; i++) 
    {
        c1 = max(c1,fabs(a[i][i]));
        c2 = max(c2,fabs(xj[i+incr][i+incr]));
    }
    my = zero;
    for (i = 1 ; i <= n ; i++) 
    {
        for (j = i ; j <= n ; j++) 
        {
             my = my+pow(a[i][j],2);
        }
    }
    my    = my/n;
    cdiag = one/sqrt(my);
    for ( i = 1 ; i <= incr ; i++) 
    {
        xj[i][i] = cdiag;   
        /* the quadratic part for the slacks       */
        /* has same order of magnitude as the part */
        /* coming from the original Lagrangian     */ 
    }
    for (i = 1 ; i <= ndual ; i++) 
    {
        if ( i >  incr ) o8qpdu_g0[i] = gradf[i-incr]*scf;
        if ( i <= incr ) o8qpdu_g0[i] = tauqp; 
        /* the linear part for the slacks */
    }
    /* compute unconstrained solution */
    /* the Cholesky-factor of a is stored in the upper triangle */
    
    for (i = 1 ; i <= n ; i++) 
    {
        su = zero;
        for (j = 1 ; j <= i-1 ; j++) 
        {
            su = su+a[j][i]*o8qpdu_y[j+incr];
        }
        o8qpdu_y[i+incr] = (o8qpdu_g0[i+incr]-su)/a[i][i];
    }
    for (i = n ; i >= 1 ; i--) 
    {
        su = zero;
        for (j = i+1 ; j <= n ; j++) 
        {
            su = su+a[i][j]*o8qpdu_y[j+incr];
        }
        o8qpdu_y[i+incr] = (o8qpdu_y[i+incr]-su)/a[i][i];
    }
    for (i = 1 ; i <= incr ; i++) 
    {
    
        /* initially assume the slacks being zero */
        
        o8qpdu_y[i] = zero;
    }
    for (i = 1 ; i <= ndual ; i++) 
    {
        o8qpdu_xd[i] = -o8qpdu_y[i];
    }
    /* unconstrained minimizer of the QP: slacks come first */
    
    f = p5*o8sc1(1,ndual,o8qpdu_g0,o8qpdu_xd);
    fmax = f ;
    /* define the initial working set: all slacks are at their */
    /* lower bounds. iq = dimension of the QP's working set    */
    
    iq = nr;

    for (i = 1 ; i <= iq ; i++) 
    {
        o8qpdu_ai[i]   = i;
        r[i][i] = one;
        ud[i]   = tauqp;
    }
    /* slacks are at zero, multipliers for the slacks at tauqp */
    rnorm = one;
    rlow  = one;
    
    /* introduction of equality constraints                       */
    /* these are characterized by a negative index in ai and never*/ 
    /* considered for inactivation. they also play no role for the*/ 
    /* dual step size t1                                          */ 
 
    
    for (i = 1 ; i <= me ; i++) 
    {
        for ( j = 1 ; j <= iq ; j ++) 
        {
            ud1[j] = ud[j];
        }
        L20:

        ud1[iq+1] = zero;
        
        /* an equality constraint is indicated by the negative index */
    
        o8qpdu_ai[iq+1] = -o8qpdu_eqlist[i] ;
        l = (o8qpdu_eqlist[i]+1)/2 ;
        /* store the gradient of this constraint in np */
        for (j = 1 ; j <= n ; j++) 
        {
          if ( l > n )
          {  /* sign is 1 for equality constraints */       
            o8qpdu_cei[j+incr] = gres[j][l-n];
          }
          else
          {
            o8qpdu_cei[j+incr] = zero ;
          }
        }
        for (j = 1 ; j <= incr ; j++) 
        {
            o8qpdu_cei[j] = zero;
        }
        o8qpdu_cei[i] = one;
        if ( res[o8qpdu_eqlist[i]] > zero ) o8qpdu_cei[i] = -one;
        if ( l <= n ) 
        {
          o8qpdu_cei[l+incr] = xsc[l] ;   /* a fixed primal variable */
        }
        for (j = 1 ; j <= ndual ; j++) 
        {
            np[j] = o8qpdu_cei[j];
        }

        o8zup(o8qpdu_z);
        
        if ( iq != 0 ) o8rup(o8qpdu_vr);
        
        /* |z| = 0? */
        
        zz   = o8vecn(1,ndual,o8qpdu_z);
        term = o8sc1(1,ndual,o8qpdu_z,np);
        
        if ( zz >= tiny*rnorm && term > zero ) 
        {
            t2 = (-o8sc1(1,ndual,np,o8qpdu_xd)-res[o8qpdu_eqlist[i]])/term;
        } 
        else if ( (-o8sc1(1,ndual,np,o8qpdu_xd)-res[o8qpdu_eqlist[i]]) >= zero ) 
        {
            t2 = infiny;
        } 
        else 
        {
            t2 = -infiny;
        }
        /* for an equality constraint t2 may be positive or negative*/
        
        if ( iq != 0 ) o8rup(o8qpdu_vr);
        l = 0;
        if ( t2 > zero ) 
        {
            t1 = infiny;
            for (k = 1 ; k <= iq ; k++) 
            {
                if( o8qpdu_vr[k] > zero && o8qpdu_ai[k] > 0 ) 
                {
                    if( ud1[k]/o8qpdu_vr[k] < t1 ) 
                    {
                        t1 = ud1[k]/o8qpdu_vr[k];
                    }
                }
            }
            t = min(t1,t2);
        } 
        else 
        {
            t1 = infiny;
            for ( k = 1 ; k <= iq ; k++) 
            {
                if( o8qpdu_vr[k] < zero && o8qpdu_ai[k] > 0 ) 
                {
                    if( ud1[k]/fabs(o8qpdu_vr[k]) < t1 ) 
                    {
                        t1 = ud1[k]/fabs(o8qpdu_vr[k]);
                    }
                }
            }
            t1 = -t1;
            t  = max(t1,t2);
            
            /* t now negative */
        }
        /* add constraint , otherwise we must first delete some */
        /* inequality constraint with zero multiplier           */
        /* first delete then add!                               */
        
        if ( fabs(t)  >= infiny ) 
        {
          qpterm = -2 ;
          if ( ! silent ) o8msg(20);
          goto L2000;
        }        
        if ( fabs(t2) >= infiny ) 
        {
        
            /* purely dual step */
            
            for (k = 1 ; k <= iq ; k++) 
            {
                ud1[k] = ud1[k]+t*(-o8qpdu_vr[k]);
                if ( ud1[k] < zero && o8qpdu_ai[k] > 0 ) ud1[k] = zero;
                /*  ud1[k] < 0 is a roundoff effect */
            }
            ud1[iq+1] = ud1[iq+1]+t;
            o8qpdu_qpdel[0]  = 0;
            for (j = 1 ; j <= iq ; j++) 
            {
                if ( ud1[j] <= tiny && o8qpdu_ai[j] > 0 ) 
                {
                    o8qpdu_qpdel[0]        = o8qpdu_qpdel[0]+1;
                    o8qpdu_qpdel[o8qpdu_qpdel[0]] = o8qpdu_ai[j];
                }
            }
            for (k = 1 ; k <= o8qpdu_qpdel[0] ; k++) 
            {
                l      = o8qpdu_qpdel[k];
                o8qpdu_iai[l] = l;
                
                o8dlcd(o8qpdu_ai,l);
            }
            goto L20;
        }
        /* primal and dual step */
        for (k = 1 ; k <= ndual ; k++) 
        {
            o8qpdu_xd[k] = o8qpdu_xd[k]+t*o8qpdu_z[k];
        }
        for (k = 1 ; k <= iq ; k++) 
        {
            ud1[k] = ud1[k]+t*(-o8qpdu_vr[k]);
            if ( ud1[k] < zero && o8qpdu_ai[k] > 0 ) ud1[k] = zero;
        }
        ud1[iq+1] = ud1[iq+1]+t;
        
        f = f+t*o8sc1(1,ndual,o8qpdu_z,np)*(p5*t+ud1[iq+1]);
        if ( f <= fmax*(one+epsmac)+epsmac )
        {
          nosucc += 1 ;
          if ( nosucc > qpitma ) 
          {
            qpterm = - 3; 
            if ( ! silent ) o8msg(26);
            goto L2000;
          }
        }
        else
        {
          nosucc = 0 ; 
        }
        fmax = max( f , fmax ) ;

        if ( fabs(t2-t1) <= tiny ) 
        {
            o8qpdu_qpdel[0] = 0;
            for (j = 1 ; j <= iq ; j++) 
            {
                if ( ud1[j] <= tiny && o8qpdu_ai[j] > 0 ) 
                {
                    o8qpdu_qpdel[0]        = o8qpdu_qpdel[0]+1;
                    o8qpdu_qpdel[o8qpdu_qpdel[0]] = o8qpdu_ai[j];
                }
            }
            for (k = 1 ; k <= o8qpdu_qpdel[0] ; k++) 
            {
                l      = o8qpdu_qpdel[k];
                o8qpdu_iai[l] = l;
                
                o8dlcd(o8qpdu_ai,l);
            }
            o8qpdu_ai[iq+1] = -o8qpdu_eqlist[i] ;
            
            o8adcd();
            
        } 
        else if ( t == t2 ) 
        {
            o8qpdu_ai[iq+1] = -o8qpdu_eqlist[i] ;
            
            o8adcd();
            
        } 
        else 
        {
            o8qpdu_qpdel[0] = 0;
            for (j = 1 ; j <= iq ; j++) 
            {
                if ( ud1[j] <= tiny && o8qpdu_ai[j] > 0 ) 
                {
                    o8qpdu_qpdel[0]        = o8qpdu_qpdel[0]+1;
                    o8qpdu_qpdel[o8qpdu_qpdel[0]] = o8qpdu_ai[j];
                }
            }
            for (k = 1 ; k <= o8qpdu_qpdel[0] ; k++) 
            {
                l      = o8qpdu_qpdel[k];
                o8qpdu_iai[l] = l;
                o8dlcd(o8qpdu_ai,l);
            }
            goto L20;
        }
        for (j = 1 ; j <= iq ; j++) 
        {
            ud[j] = ud1[j];
        }  
    /* end of loop over equality constraints */
    }

    /* set iai = k\ai */
    
    for (i = 1 ; i <= mi ; i++) 
    {
        o8qpdu_iai[i] = i;
    }
    /* step 1 */
    
    L50:

    /* ai = QP - working set , iai[i] = 0 if i in ai */
    
    for (i = 1 ; i <= iq ; i++) 
    {
        ip = o8qpdu_ai[i];
        if ( ip > 0 ) o8qpdu_iai[ip] = 0;
    }
    /* s[o8qpdu_xd] = ci(trans)*o8qpdu_xd+ci0 >= 0 ? */
    
    psid = zero;
    
    /* psid : the measure of infeasibility */
    
    for (i = 1 ; i <= mi ; i++) 
    {
    
        /* iaexcl: if = 0, exclude from addition in this cycle */
        
        o8qpdu_iaexcl[i] = 1;
        su        = zero;
        k         =  0 ;
        /* numbers of inequality constraints:                           */
        /* i = 1,...,nr corresponds to the constraints v >= 0, u_a >= 0 */
        /* i = nr+1,....,mi to the regularized general inequalities     */
        
        if ( i > nr ) 
        {
            /* an original inequality constraint */
            k = (o8qpdu_iqlist[i]-nr+1)/2 ;
            if ( k > n )
            {  /* not a bound, gradient stored in gres, sign is in gres[0][] */
              for (j = 1 ; j <= n ; j++) 
              {
                o8qpdu_cii[j+incr] = gres[j][k-n]*gres[0][k-n];
              }
              for (j = 1 ; j <= incr ; j++) 
              {
                o8qpdu_cii[j] = zero;
              }
              o8qpdu_cii[me+i-incr] = one;
              o8qpdu_ci0[i]         = res[o8qpdu_iqlist[i]-nr];
            }
            else
            {
              for ( j = 1 ; j <= ndual ; j++ )
              {
                o8qpdu_cii[j] = zero ;
              }
              o8qpdu_cii[me+i-incr] = one ;
              if ( (o8qpdu_iqlist[i]-nr) % 2 == 0 ) 
              {
                o8qpdu_cii[k+incr] = -xsc[k];
                o8qpdu_ci0[i] = res[o8qpdu_iqlist[i]-nr];
              }
              else
              {
                o8qpdu_cii[k+incr] = xsc[k];
                o8qpdu_ci0[i]      = res[o8qpdu_iqlist[i]-nr];
              }
            }
          
        }
        else
        { /* a QP slack variable */
          for (j = 1 ; j <= ndual ; j++) 
          {
             o8qpdu_cii[j] = zero;
          }
          o8qpdu_ci0[i] = zero;
          o8qpdu_cii[i] = one;
        }
/* end computation of data of mi-th QP constraint data */

        su   = o8sc1(1,ndual,o8qpdu_cii,o8qpdu_xd)+o8qpdu_ci0[i];
        o8qpdu_s[i] = su;
        psid = psid+min(zero,su);
    }

    for (i = 1 ; i <= iq ; i++) {
        o8qpdu_udold[i] = ud[i];
        o8qpdu_aiold[i] = o8qpdu_ai[i];
    }
    for (i = 1 ; i <= ndual ; i++) {
        o8qpdu_xdold[i] = o8qpdu_xd[i];
    }
    L60:
    ss = zero;
    ip = 0;
    
    /* introduce most violated inequality constraint */
    
    for (i = 1 ; i <= mi ; i++) {
        if( o8qpdu_s[i] < ss && o8qpdu_iai[i] != 0 && o8qpdu_iaexcl[i] != 0 ) {
            ss = o8qpdu_s[i];
            ip = i;
        }
    }
    if ( iq > 1 ) {
        condr = rnorm/rlow;
    } else {
        condr = one;
    }

    if ( fabs(psid) <= tiny*(c1*c2+condr) || ip == 0) {
    
        /* successful termination of QP-solver for current tauqp */
        
        if ( fabs(psid) > tiny*(c1*c2+condr) && ! silent ) o8msg(10);
        qpterm = 1;
        accinf[itstep][30] = one;
        accinf[itstep][13] = condr;
        accinf[itstep][14] = c1*c2;
        for (i = 1 ; i <= n ; i++) {
            d[i] = o8qpdu_xd[i+incr];
        }
        /* new : dnorm added */
        
        o8dnorm  = o8vecn(1,n,d);
        infeas = zero;
        
        for (i = 1 ; i <= incr ; i++) {
            infeas = infeas+fabs(o8qpdu_xd[i]);
        }
        /* L1-norm of slack variables */
        
        accinf[itstep][31] = tauqp;
        accinf[itstep][32] = infeas;
        wlow = FALSE;
        su1  = zero;
        su2  = zero;
        for (i = 1 ; i <= iq ; i++) 
        {
            if ( o8qpdu_ai[i] < 0 ) 
            {
                u[-o8qpdu_ai[i]] = ud[i];
            } 
            else 
            {
                if ( o8qpdu_ai[i] > nr ) u[o8qpdu_iqlist[o8qpdu_ai[i]]-nr] = ud[i];
            }
        }

        term1 = zero;
        for (j = 1 ; j <= n ; j++) {
            np[j] = gradf[j]*scf;
        }
        for (i = 1 ; i <= nres ; i++) 
        {
          if ( i > n )
          { 
            for (j = 1 ; j <= n ; j++) 
            {
                np[j] = np[j]-gres[j][i-n]*(u[2*i-1]-u[2*i]);
            }
          }
          else
          {
            np[i] = np[i]-xsc[i]*(u[2*i-1]-u[2*i]);
          }
        }
        
        b2n = o8vecn(1,n,np);
        
        if ( scf != zero ) b2n = b2n/scf;
        
        /* correction in the original variables */
            
        infe1 = zero;
        for (i = 1 ; i <= nr ; i++) {
            infe1 = infe1+fabs(o8qpdu_xd[i])*o8qpdu_mult[i];
        }
        if ( upsi <= delmin*nres 
            && b2n <= (gfn+one)*epsx*tp2 && phase >= 0
            && infeas <= delmin*nres ) {
        /* dual feasibility is automatic here */ 
        /* since multipliers may be incorrect for infeas != zero be careful */
        /* we consider the problem as successfully solved with reduced      */
        /* requirements. we terminate here                                  */

            for (i = 1 ; i <= n ; i++) 
            {
                d[i] = zero;
            }
            o8dnorm  = zero;
            optite = three;
            dirder = zero ;
            qpterm = 1 ;
            return;
        }
        /* there may be an additional increase of tauqp necessary again */
        if ( infe1 > (one-delta1/tauqp)*upsi &&
            ( o8vecn(1,n,d) <= min(infe1,pow(infe1,2))*tp1
            || upsi > tau0*p5 ) ) 
        {
            
        /* further increase tauqp ! */
            
            for (i = 1 ; i <= 2*nres ; i++) 
            {
                u[i]     = zero;
                slack[i] = zero;
            }
            if ( ! silent ) o8msg(17);
            if ( tauqp*taufac > taumax ) {
                if ( ! silent ) o8msg(5);
                qpterm = -1;
                accinf[itstep][30] = qpterm;
                accinf[itstep][31] = tauqp;
                accinf[itstep][32] = infeas;
                for (i = 1 ; i <= n ; i++) {
                    d[i] = zero;
                }
                o8dnorm = zero;
                
                return;
                
            } else {
                tauqp = tauqp*taufac;
                
                goto L10;
            }
        }
        /* compute new weights for the penalty-function */
        
        /* L500: */

        for (i = 1 ; i <= 2*nres ; i++) 
        {
            slack[i] = zero;
        }
        for (i = 1 ; i <= nr ; i++) 
        {
            slack[aalist[i]] = o8qpdu_xd[i];
        }
        wlow = FALSE;
        for (i = 1 ; i <= nres ; i++) 
        {
            w1[2*i-1] = w[2*i-1];
            w1[2*i] = w[2*i] ;
            if ( low[i] == up[i] ) 
            {
               if ( fabs(slack[2*i-1]) > fabs(res[2*i-1])+tiny ) 
               {
                    w1[2*i-1] = fabs(u[2*i-1]);
               } 
               else 
               {
                    w1[2*i-1] = ny*fabs(u[2*i-1])+tau;
               }
            } 
            else 
            {
               if ( o8bind[2*i-1] == 0 ) 
               {
                  w1[2*i-1] = max(w[2*i-1]*p8,tau);
               } 
               else 
               {
                  if ( res[2*i-1] >= zero && slack[2*i-1] <= tiny )
                    w1[2*i-1] = 
                    max(ny*fabs(u[2*i-1])+tau,(fabs(u[2*i-1])+w1[2*i-1])*p5);
                  if ( res[2*i-1] >= zero && slack[2*i-1] > tiny )
                    w1[2*i-1] = fabs(u[2*i-1]);
                  if ( res[2*i-1] < zero && slack[2*i-1] <= -res[2*i-1]+tiny )
                    w1[2*i-1] = 
                    max(ny*fabs(u[2*i-1])+tau,(fabs(u[2*i-1])+w1[2*i-1])*p5);
                  if ( res[2*i-1] < zero && slack[2*i-1] >  -res[2*i-1]+tiny )
                    w1[2*i-1] = fabs(u[2*i-1]);
               }
               if ( o8bind[2*i] == 0 ) 
               {
                  w1[2*i] = max(w[2*i]*p8,tau);
               } 
               else 
               {
                  if ( res[2*i] >= zero && slack[2*i] <= tiny )
                    w1[2*i] = 
                    max(ny*fabs(u[2*i])+tau,(fabs(u[2*i])+w1[2*i])*p5);
                  if ( res[2*i] >= zero && slack[2*i] > tiny )
                    w1[2*i] = fabs(u[2*i]);
                  if ( res[2*i] < zero && slack[2*i] <= -res[2*i]+tiny )
                    w1[2*i] = 
                    max(ny*fabs(u[2*i])+tau,(fabs(u[2*i])+w1[2*i])*p5);
                  if ( res[2*i] < zero && slack[2*i] >  -res[2*i]+tiny )
                    w1[2*i] = fabs(u[2*i]);
               }
            } 
            if ( w1[2*i-1] < w[2*i-1] || w1[2*i] < w[2*i] ) wlow = TRUE;
        }
        if ( wlow ) 
        {
            s1 = zero;
            s2 = zero;
            for (i = 1 ; i <= nres ; i++) 
            {
                if ( low[i] == up[i]  ) 
                {
                    s1 += w1[2*i-1]*fabs(resst[2*i-1]);
                    s2 += w1[2*i-1]*fabs(res[2*i-1]);
                } 
                else 
                {
                    s1 -= min(zero,resst[2*i-1])*w1[2*i-1];
                    s2 -= min(zero,res[2*i-1]  )*w1[2*i-1];
                    s1 -= min(zero,resst[2*i])*w1[2*i];
                    s2 -= min(zero,res[2*i]  )*w1[2*i];

                }
            }
            diff0 = (fxst-fx)*scf+(s1-s2);
            if ( diff0 >= eta*clow && itstep-lastdw >= max(5,min(n/10,20)) ) {
            
                /* accept new (diminished ) weights */
            
                lastdw = itstep;
                lastch = itstep;
                level  = diff0/iterma;
                psist  = s1;
                psi    = s2;
                for (i = 1 ; i <= 2*nres ; i++) {
                    if ( w1[i] != w[i] ) lastch = itstep;
                    w[i] = w1[i];
                }
                clow = clow+one;
                if ( clow > itstep/10 ) {
                
                    /* additional increase of eta */
                    
                    eta = eta*onep3;
                    if ( ! silent ) o8info(11);
                }
                if ( ! silent ) o8info(12);
                
                goto L1000;
            }
        }
        /* we cannot accept new weights */
        /* reset weights                */

        for (i = 1 ; i <= nres ; i++  ) 
        {
            w1[2*i-1] = w[2*i-1];
            w1[2*i  ] = w[2*i] ;
            if ( low[i] == up[i] ) 
            {
                if ( slack[2*i-1] > fabs(res[2*i-1]) ) 
                   w1[2*i-1] = fabs(u[2*i-1]);
                if ( slack[2*i-1] <= fabs(res[2*i-1]) ) 
                {
                  if ( w[2*i-1] <= fabs(u[2*i-1]) 
                       && fabs(u[2*i-1]) <= w[2*i-1]+tau ) 
                  {
                      w1[2*i-1] = w[2*i-1]+two*tau;
                  } 
                  else 
                  {
                      w1[2*i-1] = max(w[2*i-1],ny*fabs(u[2*i-1])+tau);
                  }
                }
            } 
            else 
            {
                if ( slack[2*i-1] > -min(-tiny,res[2*i-1]) && o8bind[2*i-1] == 1) 
                {
                   w1[2*i-1] = fabs(u[2*i-1]);
                } 
                else if(  o8bind[2*i-1] == 1 && 
                          slack[2*i-1] <= -min(-tiny,res[2*i-1])
                          && u[2*i-1] <= w[2*i-1]+tau && w[2*i-1] >= u[2*i-1] ) 
                {
                    
                    w1[2*i-1] = w[2*i-1]+two*tau;
                } 
                else if ( o8bind[2*i-1] == 1 ) 
                {
                    w1[2*i-1] = max(w[2*i-1],ny*fabs(u[2*i-1])+tau);
                }
                if ( slack[2*i] > -min(-tiny,res[2*i]) && o8bind[2*i] == 1 )
                {
                   w1[2*i] = fabs(u[2*i]);
                }
                else if ( o8bind[2*i] == 1 && slack[2*i] <= -min(-tiny,res[2*i])
                            && u[2*i] <= w[2*i]+tau && w[2*i] >= u[2*i] )
                {
                 
                    w1[2*i] = w[2*i]+two*tau;
                }
                else if ( o8bind[2*i] == 1 )
                {
                    w1[2*i] = max(w[2*i],ny*fabs(u[2*i])+tau);
                }

            }
        }
        term1 = zero;
        for (i = 1 ; i <= 2*nres ; i++) {
            if ( w1[i] > w[i] || w1[i] < w[i] ) lastch = itstep;
            if ( w1[i] > w[i] ) lastup = itstep;
            if ( w1[i] < w[i] ) lastdw = itstep;
            w[i]  = w1[i];
            term1 = max(term1,w[i]);
        }
        s1 = zero;
        s2 = zero;
        for (i = 1 ; i <= nres ; i++) {
            if ( low[i] == up[i] ) 
            {
                s1 += w[2*i-1]*fabs(resst[2*i-1]);
                s2 += w[2*i-1]*fabs(res[2*i-1]);
            } else {
                s1 -= w[2*i-1]*min(zero,resst[2*i-1]);
                s2 -= w[2*i-1]*min(zero,res[2*i-1]);
                s1 -= w[2*i]*min(zero,resst[2*i]);
                s2 -= w[2*i]*min(zero,res[2*i]);

            }
        }
        psist = s1;
        psi   = s2;
        if ( ! silent ) o8info(12);
        accinf[itstep][20] = term1;
        accinf[itstep][19] = clow;
        
        goto L1000;
    }
   /************************************************************/
   /**     continue QP solver                                 **/
   /************************************************************/

    if ( ip > nr ) 
    {
    /* compute gradient of current QP inequality constraint */
    /* this is a linearized original constraint */
        k = (o8qpdu_iqlist[ip]-nr+1)/2 ;
        /*  k is in {1,...,nres} */
        if ( k > n ) 
        {   /* a general constraint */
          for (j = 1 ; j <= n ; j++) 
          {
            o8qpdu_cii[j+incr] = gres[j][k-n]*gres[0][k-n];
          }
          for (j = 1 ; j <= incr ; j++) 
          {
            o8qpdu_cii[j] = zero;
          }
          o8qpdu_cii[me+ip-nr] = one;
          o8qpdu_ci0[ip]       = res[o8qpdu_iqlist[ip]-nr];
        }
        else
        {  /* an active bound constraint */
           for ( j = 1 ; j <= ndual ; j++ )
           {
             o8qpdu_cii[j] = zero ;
           }
           o8qpdu_cii[me+ip-nr] = one ;
           o8qpdu_ci0[ip      ] = res[o8qpdu_iqlist[ip]-nr];
           o8qpdu_cii[k+incr  ] = ( (o8qpdu_iqlist[ip]-nr) %2 ==0  ) ? -xsc[k] : xsc[k] ;
        }        

    } 
    else 
    {
        for (j = 1 ; j <= ndual ; j++) 
        {
            o8qpdu_cii[j] = zero;
        }
        o8qpdu_ci0[ip] = zero;
        o8qpdu_cii[ip] = one;
    }
    for (i = 1 ; i <= ndual ; i++) {
        np[i] = o8qpdu_cii[i];
    }

    for (i = 1 ; i <= iq ; i++) {
        ud1[i] = ud[i];
    }
    ud1[iq+1] = zero;
    o8qpdu_ai[iq+1]  = ip;     /* this is in {1,..,mi} */
    
    L100:

    /* step 2a */
    
    o8zup(o8qpdu_z);
    
    if ( iq != 0 ) o8rup(o8qpdu_vr);
    
    l  = 0;
    t1 = infiny;
    for (k = 1 ; k <= iq ; k++) {
        if(o8qpdu_ai[k] > 0 && o8qpdu_vr[k] > zero) {
            if ( ud1[k]/o8qpdu_vr[k] < t1 ) {
                t1 = ud1[k]/o8qpdu_vr[k];
            }
        }
    }
    /* |z| = 0? */
    
    /* old      zz = o8sc1(1,ndual,o8qpdu_z,o8qpdu_z) */
    
    zz   = o8vecn(1,ndual,o8qpdu_z);
    term = o8sc1(1,ndual,o8qpdu_z,np);
    
    /* old      if (zz != zero && term > zero ) { */
    
    if ( zz >= tiny*rnorm && term > zero ) {
        t2 = -o8qpdu_s[ip]/term;
    } else {
        t2 = infiny;
    }
    t = min(t1,t2);

    if ( t >= infiny ) 
    {
      qpterm = -2 ;
      if ( ! silent ) o8msg(20);
      goto L2000;
    }


    if( t2 >= infiny ) {
        for (k = 1 ; k <= iq ; k++) {
            ud1[k] = ud1[k]+t*(-o8qpdu_vr[k]);
            if ( ud1[k] < zero && o8qpdu_ai[k] > 0 ) ud1[k] = zero;
        }
        ud1[iq+1] = ud1[iq+1]+t;
        o8qpdu_qpdel[0]  = 0;
        for (i = 1 ; i <= iq ; i++) {
            if ( ud1[i] <= tiny && o8qpdu_ai[i] > 0 ) {
                o8qpdu_qpdel[0]        = o8qpdu_qpdel[0]+1;
                o8qpdu_qpdel[o8qpdu_qpdel[0]] = o8qpdu_ai[i];
            }
        }
        for (k = 1 ; k <= o8qpdu_qpdel[0] ; k++) {
            l      = o8qpdu_qpdel[k];
            o8qpdu_iai[l] = l;

            o8dlcd(o8qpdu_ai,l);
        }
        goto L100;
    }
    for (k = 1 ; k <= ndual ; k++) {
        o8qpdu_xd[k] = o8qpdu_xd[k]+t*o8qpdu_z[k];
    }
    for (k = 1 ; k <= iq ; k++) {
        ud1[k] = ud1[k]+t*(-o8qpdu_vr[k]);
        if ( ud1[k] < zero && o8qpdu_ai[k] > 0 ) ud1[k] = zero;
    }
    ud1[iq+1] = ud1[iq+1]+t;
    
    f = f+t*o8sc1(1,ndual,o8qpdu_z,np)*(p5*t+ud1[iq+1]);
    if ( f <= fmax*(one+epsmac)+epsmac )
    {
      nosucc += 1 ;
      if ( nosucc > qpitma )
      {
        qpterm = - 3;
        if ( ! silent ) o8msg(26);
        goto L2000;
      }
    }  
    else
    {   
      nosucc = 0 ;
    }
    fmax = max( f , fmax ) ;
    
    if ( t2 <= t1-tiny ) {
    
        /* ddual is computed by o8zup */
        
        if ( o8vecn(iq+1,ndual,ddual) < epsmac*rnorm ) {
        
            /* degeneracy: adding this constraint gives a singular working set */
            /* theoretically impossible, but due to roundoff this may occur.   */
            /* mark this constraint and try to add another one                 */
            
            iptr = ip;
            iqtr = iq;
            for (i = 1 ; i <= iq ; i++) {
                aitr[i] = o8qpdu_ai[i];
            }
            sstr  = ss;
            riitr = o8vecn(iq+1,ndual,ddual);
            
            if ( ! silent ) o8msg(19);
            o8qpdu_iaexcl[ip] = 0;
            for (i = 1 ; i <= mi ; i++) {
                o8qpdu_iai[i] = i;
            }
            for (i = 1 ; i <= iq ; i++) {
                o8qpdu_ai[i] = o8qpdu_aiold[i];
                if (o8qpdu_ai[i] > 0 ) o8qpdu_iai[o8qpdu_ai[i]] = 0;
                ud1[i] = o8qpdu_udold[i];
            }
            for (i = 1 ; i <= ndual ; i++) {
                o8qpdu_xd[i] = o8qpdu_xdold[i];
            }
            goto L60;
        }
        /* add constraint, l-pair */

        o8adcd();
        
        o8qpdu_iai[ip] = 0;
        for (i = 1 ; i <= iq ; i++) {
            ud[i] = ud1[i];
        }
        goto L50;
    }
    su = zero;
    if ( ip > nr ) 
    {
    
      /* an original linearized inequality constraint */
      k = (o8qpdu_iqlist[ip]-nr+1)/2 ;
      /*  k is in {1,...,nres} */
      if ( k > n ) 
      {   /* a general constraint */
         for (j = 1 ; j <= n ; j++) 
         {
           o8qpdu_cii[j+incr] = gres[j][k-n]*gres[0][k-n];
         }
         for (j = 1 ; j <= incr ; j++) 
         {
           o8qpdu_cii[j] = zero;
         }
         o8qpdu_cii[me+ip-nr] = one;
         o8qpdu_ci0[ip]       = res[o8qpdu_iqlist[ip]-nr];
      }
      else
      {  /* an active bound constraint */
         for ( j = 1 ; j <= ndual ; j++ )
         {
           o8qpdu_cii[j] = zero ;
         }
         o8qpdu_cii[me+ip-nr] = one ;
         o8qpdu_ci0[ip      ] = res[o8qpdu_iqlist[ip]-nr];
         o8qpdu_cii[k+incr  ] = ( (o8qpdu_iqlist[ip]-nr) %2 ==0  ) ? -xsc[k] : xsc[k] ;
      }        
      o8qpdu_s[ip] = o8sc1(1,ndual,o8qpdu_cii,o8qpdu_xd)+o8qpdu_ci0[ip];
    } 
    else 
    {
        /* a slack constraint */
        
        o8qpdu_s[ip] = o8qpdu_xd[ip];
    }
    /* now t = t1 */
    
    o8qpdu_qpdel[0] = 0;
    for (i = 1 ; i <= iq ; i++) {
        if ( ud1[i] <= tiny && o8qpdu_ai[i] > 0 ) {
            o8qpdu_qpdel[0]        = o8qpdu_qpdel[0]+1;
            o8qpdu_qpdel[o8qpdu_qpdel[0]] = o8qpdu_ai[i];
        }
    }
    for (k = 1 ; k <= o8qpdu_qpdel[0] ; k++) {
        l      = o8qpdu_qpdel[k];
        o8qpdu_iai[l] = l;
        
        o8dlcd(o8qpdu_ai,l);
    }
    if ( t2 <= t1+tiny ) {
        if ( o8vecn(iq+1,ndual,ddual) < epsmac*rnorm ) {
        
            /* degeneracy */
            
            iptr = ip;
            iqtr = iq;
            for (i = 1 ; i <= iq ; i++) {
                aitr[i] = o8qpdu_ai[i];
            }
            sstr  = ss;
            riitr = o8vecn(iq+1,ndual,ddual);
            
            if ( ! silent ) o8msg(19);
            o8qpdu_iaexcl[ip] = 0;
            for (i = 1 ; i <= mi ; i++) {
                o8qpdu_iai[i] = i;
            }
            for (i = 1 ; i <= iq ; i++) {
                o8qpdu_ai[i] = o8qpdu_aiold[i];
                if ( o8qpdu_ai[i] > 0 ) o8qpdu_iai[o8qpdu_ai[i]] = 0;
                ud1[i] = o8qpdu_udold[i];
            }
            for (i = 1 ; i <= ndual ; i++) {
                o8qpdu_xd[i] = o8qpdu_xdold[i];
            }
            goto L60;
        }
        /* add constraint, l-pair */
        
        o8adcd();
        
        o8qpdu_iai[ip] = 0;
        for (i = 1 ; i <= iq ; i++) {
            ud[i] = ud1[i];
        }
        goto L50;
        
    } else {
    
        goto L100;
    }
    /* this is the exit point of o8qpdu                                     */
    /* we either may have successful or unsuccessful termination here       */
    /* the latter with qpterm = -2 or -3, in which case it may nevertheless */
    /* be possible to use the computed d. -2 or -3 exit is theoretically    */
    /* impossible but may occur due to roundoff effects.                    */
    /* we check the directional derivative of the penalty-function now      */
    
    L1000:

    /* cut and rescale d if appropriate */
    
    o8cutd();
    
    /* compute the directional derivative dirder */
    
    o8dird();
    
    if ( dirder >= zero || ( -dirder <= epsmac*tp2*(scf*fabs(fx)+psi+one) &&
        infeas > max(upsi,nres*delmin) ) ) {
        if ( ! silent ) o8msg(18);
        if ( tauqp <= taumax/taufac ) {
            tauqp = tauqp*taufac;
            
            goto L10;
            
        } else {
            if ( ! silent ) o8msg(5);
            qpterm = -1;
            accinf[itstep][30] = qpterm;
            accinf[itstep][31] = tauqp;
            accinf[itstep][32] = infeas;
            for (i = 1 ; i <= n ; i++) {
                d[i] = zero;
            }
            o8dnorm = zero;
        }
    }
    return;
    
    L2000:

    /* QP infeasible ( in this application impossible , theoretically) */
    

    accinf[itstep][30] = -two;
    accinf[itstep][13] = condr;
    accinf[itstep][14] = c1*c2;
    for (i = 1 ; i <= n ; i++) {
        d[i] = o8qpdu_xd[i+incr];
    }
    o8dnorm = o8vecn(1,n,d);
    su1   = zero;
    for (i = 1 ; i <= incr ; i++) {
        su1 = su1+fabs(o8qpdu_xd[i]);
    }
    /* L1-norm of slack variables */
    
    accinf[itstep][32] = su1;
    wlow = FALSE;
    su1  = zero;
    su2  = zero;
    for (i = 1 ; i <= iq ; i++) {
        if ( o8qpdu_ai[i] < 0 ) {
            u[-o8qpdu_ai[i]] = ud[i];
        } else {
            if ( o8qpdu_ai[i] > nr ) u[o8qpdu_iqlist[o8qpdu_ai[i]]-nr] = ud[i];
            /*  o8qpdu_iqlist[ai[i]] = aalist[k]+nr for some k */
        }
    }
    /* compute Lagrangian violation */
    term1 = zero;
    for (j = 1 ; j <= n ; j++) {
        np[j] = gradf[j]*scf;
    }
    for (i = 1 ; i <= nres ; i++) 
    {
      if ( i > n )
      {  
        for (j = 1 ; j <= n ; j++) 
        {
          np[j] = np[j]-gres[j][i-n]*(u[2*i-1]-u[2*i]);
        }
      }
      else
      {
          np[i] -= xsc[i]*(u[2*i-1]-u[2*i]) ;
      }
      w1[2*i-1] = w[2*i-1];
      w1[2*i  ] = w[2*i] ;
      if ( low[i] == up[i]  ) 
      {
        if ( slack[2*i-1] > fabs(res[2*i-1]) ) w1[2*i-1] = fabs(u[2*i-1]);
        if ( slack[2*i-1] <= fabs(res[2*i-1]) ) 
        {
           if ( w[2*i-1] <= fabs(u[2*i-1]) 
                && fabs(u[2*i-1]) <= w[2*i-1]+tau ) 
           {
               w1[2*i-1] = w[2*i-1]+two*tau;
           } 
           else 
           {
               w1[2*i-1] = max(w[2*i-1],ny*fabs(u[2*i-1])+tau);
           }
        }
        su1 += fabs(res[2*i-1]  )*w1[2*i-1];
        su2 += fabs(resst[2*i-1])*w1[2*i-1];
      } 
      else 
      {
        if ( slack[2*i-1] > -min(-tiny,res[2*i-1]) && o8bind[2*i-1] == 1 ) 
        {
           w1[2*i-1] = fabs(u[2*i-1]);
        } 
        else if ( o8bind[2*i-1] == 1 && slack[2*i-1] <= -min(-tiny,res[2*i-1])
                  && u[2*i-1] <= w[2*i-1]+tau && w[2*i-1] >= u[2*i-1] ) 
        {
           w1[2*i-1] = w[2*i-1]+two*tau;
        } 
        else if ( o8bind[2*i-1] == 1 ) 
        {
           w1[2*i-1] = max(w[2*i-1],ny*fabs(u[2*i-1])+tau);
        }
        su1 -= w1[2*i-1]*min(zero,res[2*i-1]);
        su2 -= w1[2*i-1]*min(zero,resst[2*i-1]);
     
        if ( slack[2*i] > -min(-tiny,res[2*i]) && o8bind[2*i] == 1 ) 
        {
           w1[2*i] = fabs(u[2*i]);
        } 
        else if ( o8bind[2*i] == 1 && slack[2*i] <= -min(-tiny,res[2*i])
                  && u[2*i] <= w[2*i]+tau && w[2*i] >= u[2*i] ) 
        {
           w1[2*i] = w[2*i]+two*tau;   
        } 
        else if ( o8bind[2*i] == 1 ) 
        {
           w1[2*i] = max(w[2*i],ny*fabs(u[2*i])+tau);
        }
        su1 -= w1[2*i]*min(zero,res[2*i]);  
        su2 -= w1[2*i]*min(zero,resst[2*i]);

      }
      if ( w[i] != w1[i] ) lastch = itstep;
      w[i]  = w1[i];
      term1 = max(term1,w[i]);
    }
    psist = su2;
    psi   = su1;
    
    b2n   = sqrt(o8sc1(1,n,np,np));
    
    if ( scf != zero ) b2n = b2n/scf;
    if ( wlow ) {
        clow   = clow+one;
        lastch = itstep;
        lastdw = itstep;
    }
    if ( ! silent ) o8info(12);
    accinf[itstep][19] = clow;
    accinf[itstep][20] = term1;
    accinf[itstep][31] = tauqp;
        if ( upsi <= delmin*nres   
            && b2n <= (gfn+one)*epsx*tp2 && phase >= 0
            && infeas <= delmin*nres )
    {
     
      /* since multipliers may be incorrect for infeas != zero be careful */
      /* we consider the problem as successfully solved with reduced      */
      /* requirements                                                     */
     
            for (i = 1 ; i <= n ; i++) {
                d[i] = zero;
            }
            o8dnorm  = zero;
            optite = three;
            qpterm = 1 ;   /* new */
            return;
    }

    goto L1000;
}

/* **************************************************************************** */
/*                  compute updated projected gradient (primal)                 */
/* **************************************************************************** */
void o8zup(double z[]) {

    static int  i,j;
    static double   su;

    /* d = j(trans) *np */
    
    for (i = 1 ; i <= ndual ; i++) {
        su = zero;
        for (j = 1 ; j <= ndual ; j++) {
            su = su+xj[j][i]*np[j];
        }
        ddual[i] = su;
    }
    /* computation of z */
    
    for (i = 1 ; i <= ndual ; i++) {
        z[i] = zero;
        for (j = iq+1 ; j <= ndual ; j++) {
            z[i] = z[i]+xj[i][j]*ddual[j];
        }
    }
    return;
}

/* **************************************************************************** */
/*                    compute correction of dual multipliers                    */
/* **************************************************************************** */
void o8rup(double rv[]) {

    static double   s;
    static int  i,j;

    for (i = iq ; i >= 1 ; i--) {
        s = zero;
        for (j = i+1 ; j <= iq ; j++) {
            s = s+r[i][j]*rv[j];
        }
        rv[i] = (ddual[i]-s)/r[i][i];
    }
    return;
}

/* **************************************************************************** */
/*                          delete constraint nr. l                             */
/* **************************************************************************** */
void o8dlcd(int ai[],int l) {
    
    double o8dsq1(double a,double b);
    
    static int  qq,i,j,k;
    static double   t1,t2,cc,ss,h,c1,s1,xny;

    for (i = 1 ; i <= iq ; i++) {
        if ( ai[i] == l ) {
            qq = i;
            
            goto L10;
         }
    }
    L10:

    for (i = qq ; i <= iq-1 ; i++) {
        ai[i]  = ai[i+1];
        ud1[i] = ud1[i+1];
        for (j = 1 ; j <= ndual ; j++) {
            r[j][i] = r[j][i+1];
        }
    }
    /* L20: */

    ai[iq]    = ai[iq+1];
    ud1[iq]   = ud1[iq+1];
    ai[iq+1]  = 0;
    ud1[iq+1] = zero;
    for (j = 1 ; j <= iq ; j++) {
        r[j][iq] = zero;
    }
    iq = iq-1;
    
    if ( iq == 0 ) goto L100;
    
    for (j = qq ; j <= iq ; j++) {
        cc = r[j][j];
        ss = r[j+1][j];
        h  = o8dsq1(cc,ss);
        
        if ( h == zero ) goto L90;
        
        c1 = cc/h;
        s1 = ss/h;
        r[j+1][j] = zero;
        if ( c1 < zero ) {
            r[j][j] = -h;
            c1      = -c1;
            s1      = -s1;
        } else {
            r[j][j] = h;
        }
        xny = s1/(one+c1);
        for (k = j+1 ; k <= iq ; k++) {
            t1        = r[j][k];
            t2        = r[j+1][k];
            r[j][k]   = t1*c1+t2*s1;
            r[j+1][k] = xny*(t1+r[j][k])-t2;
        }
        for (k = 1 ; k <= ndual ; k++) {
            t1         = xj[k][j];
            t2         = xj[k][j+1];
            xj[k][j]   = t1*c1+t2*s1;
            xj[k][j+1] = xny*(xj[k][j]+t1)-t2;
        }
        L90:;
    }
    L100:

    rnorm = one;
    rlow  = one;
    
    /* in order to avoid a compiler error of hp in +op3 mode */
    
    if ( iq >= 1 ) {
        rnorm = fabs(r[1][1]);
        rlow  = fabs(r[1][1]);
        i     = 1;
        while ( i < iq ) {
            i     = i+1;
            rnorm = max(rnorm,fabs(r[i][i]));
            rlow  = min(rlow, fabs(r[i][i]));
        }
    }
    return;
}

/* **************************************************************************** */
/*                  add constraint whose gradient is given by np                */
/* **************************************************************************** */
void o8adcd(void) {
    
    double o8dsq1(double a,double b);
                    
    static int  i,j,k;
    static double   cc,ss,h,s1,c1,t1,t2,xny;

    for (j = ndual ; j >= iq+2 ; j--) {
        cc = ddual[j-1];
        ss = ddual[j];
        h  = o8dsq1(cc,ss);
        
        if ( h == zero ) goto L20;
        
        ddual[j] = zero;
        s1       = ss/h;
        c1       = cc/h;
        if ( c1 < zero ) {
            c1         = -c1;
            s1         = -s1;
            ddual[j-1] = -h;
        } else {
            ddual[j-1] = h;
        }
        xny = s1/(one+c1);
        for (k = 1 ; k <= ndual ; k++) {
            t1         = xj[k][j-1];
            t2         = xj[k][j];
            xj[k][j-1] = t1*c1+t2*s1;
            xj[k][j]   = xny*(t1+xj[k][j-1])-t2;
        }
        L20:;
    }
    iq = iq+1;
    for (i = 1 ; i <= iq ; i++) {
        r[i][iq] = ddual[i];
    }
    rnorm = one;
    rlow  = one;
    
    /* in order to avoid a compiler error of hp in +op3 mode */
    
    if ( iq >= 1 ) {
        rnorm = fabs(r[1][1]);
        rlow  = fabs(r[1][1]);
        i     = 1;
        while ( i < iq ) {
            i     = i+1;
            rnorm = max(rnorm,fabs(r[i][i]));
            rlow  = min(rlow, fabs(r[i][i]));
        }
    }
    return;
}

/* **************************************************************************** */
/*          computes the inverse of the upper triangular matrix part            */
/* **************************************************************************** */
void o8rinv(int n,double **a,int ndual,double **x) {
            
    /* computes the inverse of the upper triangular matrix part */
    /* of a and stores it in the upper triangle of the          */
    /* right lower minor of x                                   */
    /* actual dimension of a is n and of x ndual                */
    
    static int  l,j,k,incr;
    static double   su;

    incr = ndual-n;
    
    /* incr = nr */
    
    for (j = n ; j >= 1 ; j--) {
    
        /* we assume a being sufficiently regular here. given in this */
        /* application. See top of o8qpdu                             */
        
        x[j+incr][j+incr] = one/a[j][j];
        for (k = j-1 ; k >= 1 ; k--) {
            su = zero;
            for (l = k+1 ; l <= j ; l++) {
                su = su+a[k][l]*x[l+incr][j+incr];
            }
            x[k+incr][j+incr] = -su/a[k][k];
        }
    }
    return;
}

/* **************************************************************************** */
/* suite of function interfaces, for performing scaling and                     */
/* external evaluations                                                         */
/* if bloc = TRUE then it is assumed that prior to calls to es<xyz>             */
/* valid function information is computed (via a call of user_eval)             */
/* and stored in fu and fugrad , setting valid = TRUE afterwards                */
/* **************************************************************************** */

/* **************************************************************************** */
/*                              objective function                              */
/* **************************************************************************** */
void esf(double x[],double *fx) 
{

    void ef(double x[],double *fx);
    
    static int  i;
    
    if ( bloc ) 
    {
        if ( valid ) 
        {
            *fx = fu[0];
        } 
        else 
        {
            REprintf("donlp2: bloc-call, function info invalid\n");
            error("Rdonlp2 Internal Error");
        }
    } 
    else 
    {
        for (i = 1 ; i <= n ; i++) 
        {
            xtr[i] = x[i]*xsc[i];
        }
        ef(xtr,fx);
       
    }
    return;
}

/* **************************************************************************** */
/*                        gradient of objective function                        */
/* **************************************************************************** */
void esgradf(double x[],double gradf[]) 
{

    void    ef    (double x[],double *fx);
    void    egradf(double x[],double gradf[]);
    
    static int  j;
    static double   d1,d2,d3,sd1,sd2,sd3,fhelp,fhelp1,fhelp2,
                    fhelp3,fhelp4,fhelp5,fhelp6,xincr,xhelp,floc;
                            
    if ( bloc ) 
    {
        if ( valid ) 
        {
            for (j = 1 ; j <= n ; j++) 
            {
                gradf[j] = xsc[j]*fugrad[j][0];
            }
            return;
        }
        else 
        {
            REprintf("donlp2: bloc call with function info invalid\n");
            error("Rdonlp2 Internal Error");
        }
    } 
    else 
    {
        for (j = 1 ; j <= n ; j++) 
        {
            xtr[j] = xsc[j]*x[j];
        }
        if ( analyt ) 
        {
        
             egradf(xtr,gradf);
             
        } 
        else 
        {
            if ( difftype == 1 ) 
            {
                deldif = min(tm1*sqrt(epsfcn),tm2);
                
                ef(xtr,&floc);
                
                for (j = 1 ; j <= n ; j++) 
                {
                    xhelp = xtr[j];
                    xincr = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
                    if ( xhelp >= zero ) 
                    {
                        xtr[j] = xhelp+xincr;
                    }
                    else 
                    {
                        xtr[j] = xhelp-xincr;
                    }
                    ef(xtr,&fhelp);
                    
                    gradf[j] = (fhelp-floc)/(xtr[j]-xhelp);
                    xtr[j]   = xhelp;
                }
            } 
            else if ( difftype == 2 ) 
            {
                deldif = min(tm1*pow(epsfcn,one/three),tm2);
                for (j = 1 ; j <= n ; j++) 
                {
                    xhelp  = xtr[j];
                    xincr  = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
                    xtr[j] = xhelp+xincr;
                    
                    ef(xtr,&fhelp1);
                    
                    xtr[j] = xhelp-xincr;
                    
                    ef(xtr,&fhelp2);
                    
                    gradf[j] = (fhelp1-fhelp2)/(xincr+xincr);
                    xtr[j]   = xhelp;
                }
            } 
            else   /* difftype ==3 */ 
            {
                deldif = min(tm1*pow(epsfcn,one/seven),tm2);
                for (j = 1 ; j <= n ; j++) 
                {
                   xhelp  = xtr[j];
                   xincr  = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd/four);
                   xtr[j] = xhelp-xincr;
                    
                   ef(xtr,&fhelp1);
                    
                   xtr[j] = xhelp+xincr;
                    
                   ef(xtr,&fhelp2);
                    
                   xincr  = xincr+xincr;
                   d1     = xincr;
                   xtr[j] = xhelp-xincr;
                    
                   ef(xtr,&fhelp3);
                    
                   xtr[j] = xhelp+xincr;
                    
                   ef(xtr,&fhelp4);
                    
                   xincr  = xincr+xincr;
                   d2     = xincr;
                   xtr[j] = xhelp-xincr;
                   
                   ef(xtr,&fhelp5);
                    
                   xtr[j] = xhelp+xincr;
                    
                   ef(xtr,&fhelp6);
                    
                   xtr[j]   = xhelp;
                   d3       = xincr+xincr;
                   sd1      = (fhelp2-fhelp1)/d1;
                   sd2      = (fhelp4-fhelp3)/d2;
                   sd3      = (fhelp6-fhelp5)/d3;
                   sd3      = sd2-sd3;
                   sd2      = sd1-sd2;
                   sd3      = sd2-sd3;
                   gradf[j] = sd1+p4*sd2+sd3/c45;
                }
            }
        }
        for (j = 1 ; j <= n ; j++) 
        {
            gradf[j] = xsc[j]*gradf[j];
        }
    }
    return;
}

/********************************************************************/

void escon( int call_type , int liste[], double x[], double constr[],
            int errliste[])
{
void  econ(int call_type,int liste[],double  xtr[],double constr[],
            int errliste[]);
int j    ;

     if ( bloc ) 
     {
        if ( valid ) 
        {
            if ( call_type == 1 )
            {
               for ( j = 1 ; j <= nonlin ; j++ )
               {
                 constr[j] = fu [j] ;
                 errliste[j] = confuerr[j] ;
               }
            }
            else
            {
               for ( j = 1 ; j <= liste[0] ; j++ )
               {
                 constr[liste[j]] = fu[liste[j]] ;
                 errliste[liste[j]] = confuerr[liste[j]] ;
               }
            }
        }
        else
        {
            REprintf("donlp2: bloc call with function info invalid\n");
            error("Rdonlp2 Internal Error");
        }   
     } 
     else 
     {
        for (j = 1 ; j <= n ; j++) 
        {
            xtr[j] = x[j]*xsc[j];
        }
        econ(call_type,liste,xtr,constr,errliste);
     } 
     return;  
}     
void  escongrad(int liste[], int shift , double x[],
 double **grad_constr)
/********************************************************************/
/*  evaluate gradients of nonlinear constraints from liste for the  */
/*  internally scaled x                                             */     
/********************************************************************/
{
void  econ(int call_type,int liste[],double  xtr[],double constr[],
            int errliste[]);
void  econgrad(int liste[], int shift , double x[],
            double **grad_constr);
        
/* econgrad : the gradient for the external unscaled x */
static int i,j   ;
static double   d1,d2,d3,sd1,sd2,sd3,
                xincr,xhelp;

/*  after rescaling x , evaluate the gradients of the nonlinear constraints */
/*  given by liste                                                          */

    if ( bloc ) 
    {
        if ( valid ) 
        {
           for ( i = 1 ; i <= liste[0] ; i++ )
           {
             for (j = 1 ; j <= n ; j++) 
             {
                grad_constr[j][shift+liste[i]] = xsc[j]*fugrad[j][liste[i]];
             }
           }
           return;
        } 
        else 
        {
            REprintf("donlp2: bloc call with function info invalid\n");
            error("Rdonlp2 Internal Error");
        }
    } 
    else 
    {
       for (j = 1 ; j <= n ; j++) 
       {
          xtr[j] = x[j]*xsc[j];   
       }
       if ( analyt ) 
       {
        
            econgrad(liste,shift,xtr,grad_constr);
            for ( j = 1 ; j <= n ; j++ )
            {
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                grad_constr[j][shift+liste[i]] *= xsc[j] ;
              }
            }        
       } 
       else 
       {
          if ( difftype == 1 ) 
          {
            deldif = min(tm1*sqrt(epsfcn),tm2);
            econ(2,liste,xtr,escongrad_fhelp1,escongrad_errloc);
            for ( i = 1 ; i <= liste[0] ; i++ )
            {
              if ( escongrad_errloc[liste[i]] )
              {
                REprintf("donlp2: error in evaluating \n");
                REprintf("nonlinear user function %i \n",liste[i]);
                REprintf("during numerical differentiation \n");
                error("Rdonlp2 Internal Error");
              }
            }            
            for (j = 1 ; j <= n ; j++) 
            {
              xhelp = xtr[j];
              xincr = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
              if ( xhelp >= zero ) 
              {   
                 xtr[j] = xhelp+xincr;
              } 
              else 
              {
                 xtr[j] = xhelp-xincr;
              }

              econ(2,liste,xtr,escongrad_fhelp2,escongrad_errloc);  
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");
                  error("Rdonlp2 Internal Error");
                }
              }
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                grad_constr[j][shift+liste[i]] = 
                   xsc[j]*(escongrad_fhelp2[liste[i]]-escongrad_fhelp1[liste[i]])/(xtr[j]-xhelp);
              }
              xtr[j] = xhelp ;
            }
          }
          else if ( difftype == 2 )
          {
            deldif = min(tm1*pow(epsfcn,one/three),tm2);
            for (j = 1 ; j <= n ; j++) 
            {
              xhelp  = xtr[j];
              xincr  = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
              xtr[j] = xhelp+xincr;
                   
              econ(2,liste,xtr,escongrad_fhelp1,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");
                  error("Rdonlp2 Internal Error");
                }
              }
                    
              xtr[j] = xhelp-xincr;
                    
              econ(2,liste,xtr,escongrad_fhelp2,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");
                  error("Rdonlp2 Internal Error");
                }
              }

              for ( i = 1 ; i <= liste[0] ; i++)
              {      
                grad_constr[j][shift+liste[i]] = 
                    xsc[j]*(escongrad_fhelp1[liste[i]]-escongrad_fhelp2[liste[i]])/(xincr+xincr);
              }
              xtr[j]    = xhelp;
            }
          }
          else
          {
            deldif = min(tm1*pow(epsfcn,one/seven),tm2);
            for (j = 1 ; j <= n ; j++) 
            {
              xhelp  = xtr[j];
              xincr  = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd/four);
              xtr[j] = xhelp-xincr;
                    
              econ(2,liste,xtr,escongrad_fhelp1,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  

              xtr[j] = xhelp+xincr;
              econ(2,liste,xtr,escongrad_fhelp2,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  
                    
              xincr  = xincr+xincr;
              d1     = xincr;
              xtr[j] = xhelp-xincr;
              econ(2,liste,xtr,escongrad_fhelp3,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  

              xtr[j] = xhelp+xincr;
              econ(2,liste,xtr,escongrad_fhelp4,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  
                                        
              xincr  = xincr+xincr;
              d2     = xincr;
              xtr[j] = xhelp-xincr;
                    
              econ(2,liste,xtr,escongrad_fhelp5,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  
                                        
              xtr[j] = xhelp+xincr;
              econ(2,liste,xtr,escongrad_fhelp6,escongrad_errloc);
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                if ( escongrad_errloc[liste[i]] )
                {
                  REprintf("donlp2: error in evaluating \n");
                  REprintf("nonlinear user function %i \n",liste[i]);
                  REprintf("during numerical differentiation \n");   
                  error("Rdonlp2 Internal Error");
                }
              }  

              xtr[j]    = xhelp;
              d3        = xincr+xincr;
              for ( i = 1 ; i <= liste[0] ; i++ )
              {
                 sd1       = (escongrad_fhelp2[liste[i]]-escongrad_fhelp1[liste[i]])/d1;
                 sd2       = (escongrad_fhelp4[liste[i]]-escongrad_fhelp3[liste[i]])/d2;
                 sd3       = (escongrad_fhelp6[liste[i]]-escongrad_fhelp5[liste[i]])/d3;
                 sd3       = sd2-sd3;
                 sd2       = sd1-sd2;
                 sd3       = sd2-sd3; 
                 grad_constr[j][shift+liste[i]] =
                 xsc[j]*(sd1+p4*sd2+sd3/c45);
              }  /* end for i */
            } /* end for j */
          }
       } /* end if analyt else */
    } /* end if bloc else */
    return;
}
/* **************************************************************************** */
/* suite of functions to allocate and free memory for dynamic memory allocation */
/* **************************************************************************** */

#ifdef ARRAY_BORDER_CHECK
/**********************************************************************/
/***           check 1D array borders for any violations        *******/
/**********************************************************************/
void checkArrayBorders(char *str) 
{
    int i, violations;
    violations = 0;

    /* Check integer borders */
    for (i=0; i<i_borders_count; i++) {
        if (**(i_borders+2*i) != i_unique) {
           Rprintf("checkArrayBorders: Integer 1D array index %4d:  start border violated\n", i);  
            
            violations++;
        }
        if (**(i_borders+2*i+1) != i_unique) {
           Rprintf(" checkArrayBorders: Integer 1D array index %4d:  end border violated\n", i);  
            
            violations++;
        }
    }
    
    /* Check double borders */
    for (i=0; i<d_borders_count; i++) {
        if (**(d_borders+2*i) != d_unique) {
           Rprintf("checkArrayBorders: Double  1D array index %4d:  start border violated\n", i);  
            
            violations++;
        }
        if (**(d_borders+2*i+1) != d_unique) {
           Rprintf("checkArrayBorders: Double  1D array index %4d:  end border violated\n", i);  
            
            violations++;
        }
    }
    
    /* Check logical borders */
    for (i=0; i<l_borders_count; i++) {
        if (**(l_borders+2*i) != l_unique) {
           Rprintf(" checkArrayBorders: Logical 1D array index %4d:  start border violated\n", i);  
            
            violations++;
        }
        if (**(l_borders+2*i+1) != l_unique) {
           Rprintf(" checkArrayBorders: Logical 1D array index %4d:  end border violated\n", i);  
            
            violations++;
        }
    }

    if (violations == 0) 
       Rprintf("checkArrayBorders: no violations: %s\n", str);
    else
       Rprintf("checkArrayBorders: ************** violations %4d:  %s\n", violations, str);
    
}
#endif


/**********************************************************************/
/***              allocate memory for integer 1D array          *******/
/**********************************************************************/

int* i1_malloc(int size1, int init)
{

    /* Allocate int precision array with length "size1"          */
    /* assuming zero origin.  If initialize is non zero             */
    /* then initize the allocated array to zero.                    */   

    int* array;
    int i/*,j*/;
    
    array = (int*) malloc((size_t) (size1+2*ABC)*sizeof(int));
    if (!array) {
        REprintf( "ERROR: i1_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }

#ifdef ARRAY_BORDER_CHECK
    /* setup array borders */
    if (i_borders_count > 2*ABC_NUM_1DARRAYS-4) {
       Rprintf("ERROR: ARRAYBORDERS: ABC_NUM_1DARRAYS is too small\n");
    error("Rdonlp2 Internal Error");
    }   
    array[0] = i_unique;
    i_borders[2*i_borders_count] = &(array[0]);
    array[size1+1] = i_unique;
    i_borders[2*i_borders_count+1] = &(array[size1+1]);
    i_borders_count++;
    array++;
#endif    

    /* initialize the array to 0 */
    if (init) {
        for (i=0; i<size1; i++) 
            array[i] = 0;
    }

    return array;
}


/**********************************************************************/
/***                free memory for int 1D array             *******/
/**********************************************************************/
void i1_free(int* array) 
{
    /* int i; */

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: i1_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the int 1D array */
    free(array-ABC);
}

/**********************************************************************/
/***              allocate memory for int 2D array           *******/
/**********************************************************************/
int** i2_malloc(int size1, int size2, int init) 
{

    /* Allocate int precision array with lengths "size1" and     */
    /* size2 assuming zero origin.  If initialize is non zero       */
    /* then initize the allocated array to zero.                    */   

    int** array;
    int* arraytemp;
    int i,j;
    
    array = (int**) malloc((size_t) size1*sizeof(int*));
    if (!array) {
        REprintf( "ERROR: d2_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }
    for (i=0; i<size1; i++) {
        arraytemp = (int*) malloc((size_t) (size2+2*ABC)*sizeof(int));
        if (!arraytemp) {  
            REprintf( "ERROR: d2_malloc: memory error: malloc failed");  
            error("Rdonlp2 Internal Error");
        }
    
#ifdef ARRAY_BORDER_CHECK
        /* setup array borders */
        if (i_borders_count > 2*ABC_NUM_1DARRAYS-4) {
           Rprintf("ERROR: ARRAY_BORDERS_CHECK: ABC_NUM_1DARRAYS is too small\n");
        error("Rdonlp2 Internal Error");
        }   
        arraytemp[0] = i_unique;
        i_borders[2*i_borders_count] = &(arraytemp[0]);
        arraytemp[size2+1] = i_unique;
        i_borders[2*i_borders_count+1] = &(arraytemp[size2+1]);
        i_borders_count++;
#endif
        array[i] = arraytemp + ABC;
    }

    if (init) {
       for (i=0; i<size1; i++) 
           for (j=0; j<size2; j++) 
               array[i][j] = 0.0;
    }

    return array;
}

/**********************************************************************/
/***                free memory for int 2D array             *******/
/**********************************************************************/
void i2_free(int** array, int size1) 
{
    int i;

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: d2_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the int 2D array piece by piece */
    for (i=0; i<size1; i++) 
        free(array[i]-ABC);
    free(array);
}


/**********************************************************************/
/***              allocate memory for double 1D array           *******/
/**********************************************************************/
double* d1_malloc(int size1, int init) 
{

    /* Allocate double precision array with length "size1"          */
    /* assuming zero origin.  If initialize is non zero             */
    /* then initize the allocated array to zero.                    */   

    double* array;
    int i/*,j*/;
    
    array = (double*) malloc((size_t) (size1+2*ABC)*sizeof(double));
    if (!array) {
        REprintf( "ERROR: d1_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }

#ifdef ARRAY_BORDER_CHECK
    /* setup array borders */
    if (d_borders_count > 2*ABC_NUM_1DARRAYS-4) {
       Rprintf("ERROR: ARRAYBORDERS: ABC_NUM_1DARRAYS is too small\n");
    error("Rdonlp2 Internal Error");
    }   
    array[0] = d_unique;
    d_borders[2*d_borders_count] = &(array[0]);
    array[size1+1] = d_unique;
    d_borders[2*d_borders_count+1] = &(array[size1+1]);
    d_borders_count++;
    array++;
#endif

    if (init) {
        for (i=0; i<size1; i++) 
            array[i] = 0.0;
    }

    return array;
}

/**********************************************************************/
/***                free memory for double 1D array             *******/
/**********************************************************************/
void d1_free(double* array) 
{
    /* int i; */

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: d1_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the double 1D array */
    free(array-ABC); 
}

/**********************************************************************/
/***              allocate memory for double 2D array           *******/
/**********************************************************************/
double** d2_malloc(int size1, int size2, int init) 
{

    /* Allocate double precision array with lengths "size1" and     */
    /* size2 assuming zero origin.  If initialize is non zero       */
    /* then initize the allocated array to zero.                    */   

    double** array;
    double* arraytemp;
    int i,j;
    
    array = (double**) malloc((size_t) size1*sizeof(double*));
    if (!array) {
        REprintf( "ERROR: d2_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }
    for (i=0; i<size1; i++) {

        /* Allocate memory */
        arraytemp = (double*) malloc((size_t) (size2+2*ABC)*sizeof(double));
        if (!arraytemp) {  
            REprintf( "ERROR: d2_malloc: memory error: malloc failed");  
            error("Rdonlp2 Internal Error");
        }

#ifdef ARRAY_BORDER_CHECK
        /* setup array borders */
        if (d_borders_count > 2*ABC_NUM_1DARRAYS-4) {
           Rprintf("ERROR: ARRAY_BORDERS_CHECK: ABC_NUM_1DARRAYS is too small\n");
        error("Rdonlp2 Internal Error");
        }   
        arraytemp[0] = d_unique;
        d_borders[2*d_borders_count] = &(arraytemp[0]);
        arraytemp[size2+1] = d_unique;
        d_borders[2*d_borders_count+1] = &(arraytemp[size2+1]);
        d_borders_count++;
#endif
        array[i] = arraytemp + ABC;
    }

    if (init) {
       for (i=0; i<size1; i++) 
           for (j=0; j<size2; j++) 
               array[i][j] = 0.0;
    }

    return array;
}

/**********************************************************************/
/***                free memory for double 2D array             *******/
/**********************************************************************/
void d2_free(double** array, int size1) 
{
    int i;

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: d2_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the double 2D array piece by piece */
    for (i=0; i<size1; i++) 
        free(array[i]-ABC);
    free(array);
}

/**********************************************************************/
/***              allocate memory for int 1D array           *******/
/**********************************************************************/
int* l1_malloc(int size1, int init) 
{

    /* Allocate int precision array with length "size1"          */
    /* assuming zero origin.  If initialize is non zero             */
    /* then initize the allocated array to zero.                    */   

    int* array;
    int i/*,j*/;
    
    array = (int*) malloc((size_t) (size1+2*ABC)*sizeof(int));
    if (!array) {
        REprintf( "ERROR: l1_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }

#ifdef ARRAY_BORDER_CHECK
    /* setup array borders */
    if (i_borders_count > 2*ABC_NUM_1DARRAYS-4) {
       Rprintf("ERROR: ARRAYBORDERS: ABC_NUM_1DARRAYS is too small\n");
    error("Rdonlp2 Internal Error");
    }   
    array[0] = i_unique;
    i_borders[2*i_borders_count] = &(array[0]);
    array[size1+1] = i_unique;
    i_borders[2*i_borders_count+1] = &(array[size1+1]);
    i_borders_count++;
    array++;
#endif

    if (init) {
        for (i=0; i<size1; i++) 
            array[i] = 0.0;
    }

    return array;
}

/**********************************************************************/
/***                free memory for int 1D array             *******/
/**********************************************************************/
void l1_free(int* array) 
{
    /* int i; */

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: l1_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the int 1D array */
    free(array-ABC);
}

/**********************************************************************/
/***              allocate memory for int 2D array           *******/
/**********************************************************************/
int** l2_malloc(int size1, int size2, int init)
{

    /* Allocate int precision array with lengths "size1" and     */
    /* size2 assuming zero origin.  If initialize is non zero       */
    /* then initize the allocated array to zero.                    */   

    int** array;
    int* arraytemp;
    int i,j;
    
    array = (int**) malloc((size_t) size1*sizeof(int*));
    if (!array) {
        REprintf( "ERROR: l2_malloc: memory error: malloc failed");  
        error("Rdonlp2 Internal Error");
    }
    for (i=0; i<size1; i++) {
        arraytemp = (int*) malloc((size_t) (size2+2*ABC)*sizeof(int));
        if (!arraytemp) {  
            REprintf( "ERROR: l2_malloc: memory error: malloc failed");  
            error("Rdonlp2 Internal Error");
        }
    
#ifdef ARRAY_BORDER_CHECK
        /* setup array borders */
        if (i_borders_count > 2*ABC_NUM_1DARRAYS-4) {
           Rprintf("ERROR: ARRAY_BORDERS_CHECK: ABC_NUM_1DARRAYS is too small\n");
        error("Rdonlp2 Internal Error");
        }   
        arraytemp[0] = i_unique;
        i_borders[2*i_borders_count] = &(arraytemp[0]);
        arraytemp[size2+1] = i_unique;
        i_borders[2*i_borders_count+1] = &(arraytemp[size2+1]);
        i_borders_count++;
#endif
        array[i] = arraytemp + ABC;
    }

    if (init) {
       for (i=0; i<size1; i++) 
           for (j=0; j<size2; j++) 
               array[i][j] = 0.0;
    }

    return array;
}

/**********************************************************************/
/***                free memory for int 2D array             *******/
/**********************************************************************/
void l2_free(int** array, int size1) 
{
    int i;

    /* Check for null pointer */
    if (!array) {
        REprintf( "ERROR: l2_free: memory error: pointer is null");  
        error("Rdonlp2 Internal Error");
    }

    /* Free the memory for the int 2D array piece by piece */
    for (i=0; i<size1; i++) 
        free(array[i]-ABC);
    free(array);
}


/**********************************************************************/
/***              allocate memory for global arrays            *******/
/**********************************************************************/
void global_mem_malloc() {

    /* o8comm.h */
    accinf     = d2_malloc(iterma+1, 33, 1);
    x          = d1_malloc(n+1, 1);
    x0         = d1_malloc(n+1, 1);
    x1         = d1_malloc(n+1, 1);
    xmin       = d1_malloc(n+1, 1);
    d          = d1_malloc(n+1, 1);
    d0         = d1_malloc(n+1, 1);
    dd         = d1_malloc(n+1, 1);
    difx       = d1_malloc(n+1, 1);
    resmin     = d1_malloc(2*(n+nlin+nonlin+1), 1);
    gradf      = d1_malloc(n+1, 1);
    qgf        = d1_malloc(n+1, 1); 
    gphi0      = d1_malloc(n+1, 1);
    gphi1      = d1_malloc(n+1, 1);
    gres       = d2_malloc(n+1, nlin+nonlin+1, 1);
    gresn      = d1_malloc(nlin+nonlin+1, 1);
    perm       = i1_malloc(n+1, 1);
    perm1      = i1_malloc(n+1, 1);
    colno      = i1_malloc(2*(n+nlin+nonlin)+1, 1);
    qr         = d2_malloc(n+1, n+nlin+nonlin+1, 1);    
    betaq      = d1_malloc(n+nlin+nonlin+1, 1); 
    diag       = d1_malloc(n+nlin+nonlin+1, 1);
    cscal      = d1_malloc(n+nlin+nonlin+1, 1);
    colle      = d1_malloc(n+nlin+nonlin+1, 1);

    if (calc_hessian){
      hess     = d2_malloc(n+1, n+1, 1); /* added by RT */
    }
    
    a          = d2_malloc(n+1, n+1, 1);
    diag0      = d1_malloc(n+1, 1);
    violis     = i1_malloc(2*nstep*(n+nlin+nonlin)+1, 1);
    alist      = i1_malloc(n+nlin+nonlin+1, 1);
    o8bind     = i1_malloc(2*(n+nlin+nonlin)+1, 1);
    o8bind0    = i1_malloc(2*(n+nlin+nonlin)+1, 1);
    aalist     = i1_malloc(n+nlin+nonlin+1, 1);
    clist      = i1_malloc(nlin+nonlin+1, 1);  
    u          = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    u0         = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    w          = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    w1         = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    res        = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    res0       = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    res1       = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    resst      = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    yu         = d1_malloc(n+nlin+nonlin+1, 1);
    slack      = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    work       = d1_malloc(2*(n+nlin+nonlin)+1, 1);
    ug         = d1_malloc(n+1, 1);
    og         = d1_malloc(n+1, 1);
    low        = d1_malloc(n+nlin+nonlin+1, 1);
    up         = d1_malloc(n+nlin+nonlin+1, 1);
    xst        = d1_malloc(n+1, 1);
        

    /* o8fint.h */
    xtr        = d1_malloc(n+1, 1);
    xsc        = d1_malloc(n+1, 1);
    fu         = d1_malloc(nlin+nonlin+1, 1);
    fugrad     = d2_malloc(n+1, nlin+nonlin+1, 1);
    fud        = d2_malloc(nlin+nonlin+1, 7, 1);
    
    /* o8fuco.h */
    val        = l1_malloc(nlin+nonlin+1, 1);
    llow       = l1_malloc(n+1, 1);
    lup        = l1_malloc(n+1, 1);
    cres       = i1_malloc(nlin+nonlin+1, 1);
    cgres      = i1_malloc(nlin+nonlin+1, 1);
    confuerr   = l1_malloc(nlin+nonlin+1, 1);
    nonlinlist = i1_malloc(nlin+nonlin+1, 1);
    gunit      = i2_malloc(4, nlin+nonlin+1, 1);
    gconst     = l1_malloc(nlin+nonlin+1, 1);
    cfuerr     = l1_malloc(nlin+nonlin+1, 1);

    /* o8gene.h */
    np         = d1_malloc(ndualm+1, 1);
    xj         = d2_malloc(ndualm+1, ndualm+1, 1);
    ddual      = d1_malloc(ndualm+1, 1);
    r          = d2_malloc(ndualm+1, ndualm+1, 1);
    ud         = d1_malloc(mdualm+1, 1);
    ud1        = d1_malloc(mdualm+1, 1);
    aitr       = i1_malloc(mdualm+1, 1);
 
    
    /* GLOBAL VARIABLES USED LOCALLY */
    
    donlp2_yy     = d1_malloc(n+1, 1);
    o8bfgs_dg     = d1_malloc(n+1, 1); 
    o8bfgs_adx    = d1_malloc(n+1, 1);
    o8bfgs_ltdx   = d1_malloc(n+1, 1); 
    o8bfgs_gtdx   = d1_malloc(n+nlin+nonlin+1, 1);  
    o8bfgs_updx   = d1_malloc(n+1, 1);
    o8bfgs_updz   = d1_malloc(n+1, 1);
    o8opti_qtx    = d1_malloc(n+1, 1);
    o8opti_yy     = d1_malloc(n+1, 1);
    o8opti_yx     = d1_malloc(n+1, 1);
    o8opti_trvec  = d1_malloc(n+1, 1);
    o8opti_con    = d1_malloc(nlin+nonlin+1, 1);
    o8opti_delist = i1_malloc(n+nlin+nonlin+1, 1);
    o8opti_bindba = i1_malloc(2*(n+nlin+nonlin)+1, 1);
    o8eval_con    = d1_malloc(nlin+nonlin+1, 1);
    o8unim_step   = d1_malloc(nstep+1, 1);
    o8dec_qri     = d1_malloc(n+1, 1);
    o8dec_qri0    = d1_malloc(n+1, 1);
    o8elim_qri    = d1_malloc(n+1, 1);
    o8elim_y      = d1_malloc(n+1, 1);
    o8elim_rhsscal= d1_malloc(n+nlin+1, 1);
    o8elim_col    = i1_malloc(n+nlin+1, 1); 
    o8sol_xl      = d1_malloc(n+1, 1);
    o8upd_sdiag   = d1_malloc(n+1, 1);    
    o8upd_rn1     = d1_malloc(n+1, 1);
    o8upd_w       = d1_malloc(n+1, 1); 
    o8qpdu_g0     = d1_malloc(ndualm+1, 1); 
    o8qpdu_ci0    = d1_malloc(mdualm+1, 1); 
    o8qpdu_cii    = d1_malloc(mdualm+1, 1); 
    o8qpdu_cei    = d1_malloc(ndualm+1, 1); 
    o8qpdu_xd     = d1_malloc(ndualm+1, 1); 
    o8qpdu_s      = d1_malloc(mdualm+1, 1); 
    o8qpdu_z      = d1_malloc(ndualm+1, 1); 
    o8qpdu_vr     = d1_malloc(mdualm+1, 1); 
    o8qpdu_xdold  = d1_malloc(ndualm+1, 1); 
    o8qpdu_udold  = d1_malloc(mdualm+1, 1); 
    o8qpdu_ai     = i1_malloc(mdualm+1, 1);
    o8qpdu_iai    = i1_malloc(mdualm+1, 1);  
    o8qpdu_iaexcl = i1_malloc(mdualm+1, 1);
    o8qpdu_aiold  = i1_malloc(mdualm+1, 1);
    o8qpdu_eqlist = i1_malloc(n+1, 1);
    o8qpdu_iqlist = i1_malloc(mdualm+1, 1);
    o8qpdu_y      = d1_malloc(ndualm+1, 1);
    o8qpdu_mult   = d1_malloc(n+nlin+nonlin+1, 1);
    o8qpdu_qpdel  = i1_malloc(mdualm+1, 1);
    escongrad_errloc = i1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp1 = d1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp2 = d1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp3 = d1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp4 = d1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp5 = d1_malloc(nlin+nonlin+1, 1);
    escongrad_fhelp6 = d1_malloc(nlin+nonlin+1, 1);

}

/**********************************************************************/
/***                free memory from global arrays              *******/
/**********************************************************************/
void global_mem_free() {

    /* GLOBAL VARIABLES USED GLOBALLY */

    /* o8comm.h */
    d2_free(accinf, iterma+1);
    d1_free(x);
    d1_free(x0);
    d1_free(x1);
    d1_free(xmin);
    d1_free(d);
    d1_free(d0);
    d1_free(dd);
    d1_free(difx);
    d1_free(resmin);
    d1_free(gradf);
    d1_free(qgf);
    d1_free(gphi0);
    d1_free(gphi1); 
    d2_free(gres, n+1);
    d1_free(gresn); 
    i1_free(perm);  
    i1_free(perm1); 
    i1_free(colno); 
    d2_free(qr, n+1);
    d1_free(betaq);
    d1_free(diag); 
    d1_free(cscal);
    d1_free(colle);

    if (calc_hessian){
      d2_free(hess, n+1); /* added by RT */
    }
    
    d2_free(a, n+1); 
    d1_free(diag0);  
    i1_free(violis); 
    i1_free(alist);  
    i1_free(o8bind);   
    i1_free(o8bind0);  
    i1_free(aalist); 
    i1_free(clist);  
    d1_free(u);      
    d1_free(u0);     
    d1_free(w);      
    d1_free(w1);     
    d1_free(res);    
    d1_free(res0);   
    d1_free(res1);   
    d1_free(resst);  
    d1_free(yu);     
    d1_free(slack); 
    d1_free(work);  
    d1_free(ug);    
    d1_free(og);    
    d1_free(low);   
    d1_free(up);    
    d1_free(xst);   
        

    /* o8fint.h */
    d1_free(xtr);
    d1_free(xsc);
    d1_free(fu);
    d2_free(fugrad, n+1);
    d2_free(fud, nlin+nonlin+1);
    
    /* o8fuco.h */
    l1_free(val); 
    l1_free(llow);
    l1_free(lup); 
    i1_free(cres);
    i1_free(cgres);
    l1_free(confuerr);
    i1_free(nonlinlist);
    i2_free(gunit, 4);
    l1_free(gconst);
    l1_free(cfuerr);

    /* o8gene.h */
    d1_free(np);
    d2_free(xj, ndualm+1);
    d1_free(ddual);
    d2_free(r, ndualm+1);
    d1_free(ud);
    d1_free(ud1);
    i1_free(aitr); 
    
    /* GLOBAL VARIABLES USED LOCALLY */
    d1_free(donlp2_yy); 
    d1_free(o8bfgs_dg); 
    d1_free(o8bfgs_adx);
    d1_free(o8bfgs_ltdx);
    d1_free(o8bfgs_gtdx); 
    d1_free(o8bfgs_updx); 
    d1_free(o8bfgs_updz); 
    d1_free(o8opti_qtx); 
    d1_free(o8opti_yy);  
    d1_free(o8opti_yx);  
    d1_free(o8opti_trvec); 
    d1_free(o8opti_con);  
    i1_free(o8opti_delist); 
    i1_free(o8opti_bindba); 
    d1_free(o8eval_con);   
    d1_free(o8unim_step);  
    d1_free(o8dec_qri);    
    d1_free(o8dec_qri0);
    d1_free(o8elim_qri);
    d1_free(o8elim_y);
    d1_free(o8elim_rhsscal);
    i1_free(o8elim_col);   
    d1_free(o8sol_xl);  
    d1_free(o8upd_sdiag);
    d1_free(o8upd_rn1);  
    d1_free(o8upd_w);  
    d1_free(o8qpdu_g0);  
    d1_free(o8qpdu_ci0); 
    d1_free(o8qpdu_cii); 
    d1_free(o8qpdu_cei); 
    d1_free(o8qpdu_xd);  
    d1_free(o8qpdu_s); 
    d1_free(o8qpdu_z); 
    d1_free(o8qpdu_vr);  
    d1_free(o8qpdu_xdold); 
    d1_free(o8qpdu_udold); 
    i1_free(o8qpdu_ai);    
    i1_free(o8qpdu_iai);   
    i1_free(o8qpdu_iaexcl);
    i1_free(o8qpdu_aiold); 
    i1_free(o8qpdu_eqlist);
    i1_free(o8qpdu_iqlist);
    d1_free(o8qpdu_y); 
    d1_free(o8qpdu_mult); 
    i1_free(o8qpdu_qpdel); 
    i1_free(escongrad_errloc);
    d1_free(escongrad_fhelp1);
    d1_free(escongrad_fhelp2);
    d1_free(escongrad_fhelp3);
    d1_free(escongrad_fhelp4);
    d1_free(escongrad_fhelp5);
    d1_free(escongrad_fhelp6);
}

/* added by RT to calculate numeric Hessian */
void eshessf(double x[], double **hess)
{
  void    ef    (double x[],double *fx);
  void    egradf(double x[],double gradf[]);
    
  static int  j, k;
  static double   d1,d2,d3,sd1,sd2,sd3,xhelp,xincr;
  static double *gloc, *ghelp, *ghelp1, *ghelp2, *ghelp3,
    *ghelp4, *ghelp5, *ghelp6;

  if ( bloc ){
    error("eshessf(): not implemented for bloc == TRUE");
  }
  
  for (j = 1 ; j <= n ; j++){
    xtr[j] = xsc[j]*x[j];
  }
  
  switch(difftype){
  case 1:
    gloc  = (double *)Calloc(n+1, double);
    ghelp = (double *)Calloc(n+1, double);
        
    deldif = min(tm1*sqrt(epsfcn),tm2);
    esgradf(xtr, gloc);
    for (j=1; j<=n; j++){
      xhelp = xtr[j];
      xincr = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
      if ( xhelp >= zero ){
    xtr[j] = xhelp+xincr;
      }
      else{
    xtr[j] = xhelp-xincr;
      }

      esgradf(xtr, ghelp);

      for (k=1; k<=n; k++){
    hess[k][j] = (ghelp[k]-gloc[k])/(xtr[j]-xincr);
      }
      xtr[j] = xhelp;
    }
      
    Free(gloc);
    Free(ghelp);
    break;
      
  case 2:
    ghelp1 = (double *)Calloc(n+1, double);
    ghelp2 = (double *)Calloc(n+1, double);
      
    deldif = min(tm1*pow(epsfcn,one/three),tm2);
    for (j=1; j<=n; j++){
      xhelp = xtr[j];
      xincr = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd);
      xtr[j] = xhelp+xincr;
      esgradf(xtr, ghelp1);
      xtr[j] = xhelp-xincr;
      esgradf(xtr, ghelp2);
      for (k=1; k<=n; k++){
    hess[k][j] = (ghelp1[k]-ghelp2[k])/(xincr+xincr);
      }
      xtr[j] = xhelp;
    }

    Free(ghelp1);
    Free(ghelp2);
    break;
      
  case 3:
    ghelp1 = (double *)Calloc(n+1, double);
    ghelp2 = (double *)Calloc(n+1, double);
    ghelp3 = (double *)Calloc(n+1, double);
    ghelp4 = (double *)Calloc(n+1, double);
    ghelp5 = (double *)Calloc(n+1, double);
    ghelp6 = (double *)Calloc(n+1, double);

    deldif = min(tm1*pow(epsfcn,one/seven),tm2);
    for (j=1; j<=n; j++){
      xhelp = xtr[j];
      xincr = min(min(tm2,deldif*fabs(xhelp)+deldif),taubnd/four);

      xtr[j] = xhelp-xincr;
      esgradf(xtr, ghelp1);

      xtr[j] = xhelp+xincr;
      esgradf(xtr, ghelp2);

      xincr = xincr+xincr;
      d1 = xincr;
      xtr[j] = xhelp-xincr;
      esgradf(xtr, ghelp3);

      xtr[j] = xhelp+xincr;
      esgradf(xtr, ghelp4);

      xincr = xincr+xincr;
      d2 = xincr;
      xtr[j] = xhelp-xincr;
      esgradf(xtr, ghelp5);

      xtr[j] = xhelp+xincr;
      esgradf(xtr, ghelp6);

      xtr[j] = xhelp;
      d3 = xincr+xincr;
      
      for (k=1; k<=n; k++){
    sd1 = (ghelp2[k]-ghelp1[k])/d1;
    sd2 = (ghelp4[k]-ghelp3[k])/d2;
    sd3 = (ghelp6[k]-ghelp5[k])/d3;
    sd3 = sd2 - sd3;
    sd2 = sd1 - sd2;
    sd3 = sd2 - sd3;
    hess[k][j] = sd1+p4*sd2+sd3/c45;
      }
    }
    Free(ghelp1);
    Free(ghelp2);
    Free(ghelp3);
    Free(ghelp4);
    Free(ghelp5);
    Free(ghelp6);
    break;
  default:
    error("BUG in eshess(): contact the author.");
  }

  for (j=1; j<=n; j++){
    for (k=1; k<=n; k++){
      hess[k][j] = xsc[j]*hess[k][j];
    }
  }

  return;
}
