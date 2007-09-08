/* **************************************************************************** */
/*                                  o8fuco.h                                    */
/* **************************************************************************** */
    
static int   *val,*llow,*lup;

X int   n,nr,nres,nlin,nonlin, nstep, ndualm, mdualm;

static double    epsmac,tolmac,deldif;

X char      name[41];

X double    epsdif;

X int   intakt,te0,te1,te2,te3,singul;
static int   ident,eqres;
X int silent,fsilent,analyt,cold;

static int   icf,icgf,cfincr,*cres,*cgres;

X int   ffuerr,*confuerr;

/*  special variables for the interface to old fashioned function */
/*  specification                                                 */
/*  can be removed is only problems in the new formulation are to be used */
/* static int nh,ng; */

static int *nonlinlist;

static  int **gunit; 

static int *gconst; 

static int *cfuerr; 
/* this is necessary because of the different index positions used */
/*  in the old and new versions   */
