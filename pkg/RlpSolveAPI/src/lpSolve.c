#include "R.h"
#include "lp_lib.h"

void linprog(double* obj, double* A, int* pldA, int* pp, int* peq, double* b,
             double* lb, double* ub, int* pnInts, int* intvec,
             int* pPivotingRule, int* pSimplexType, int* basis,
             int* pepslevel, double* eps, int* ppresolve,
             double* pOptimalSolution, double* x, int* pStatus)
{
  lprec* lp = NULL;
  int i = 0, j = 0, ldA = -1, p = -1, eq = -1, nInts = -1, pivotingRule = -1;
  int simplexType = -1, epslevel = -1, presolve = -1, ret = -1023;
  unsigned char check = FALSE;
  double lpsInfinity = 0.0;
  int* rowno = NULL;

  ldA = *pldA;
  p = *pp;
  eq = *peq;
  nInts = *pnInts;
  pivotingRule = *pPivotingRule;
  simplexType = *pSimplexType;
  epslevel = *pepslevel;
  presolve = *ppresolve;

  rowno = (int*) R_alloc(ldA, sizeof(int));
  for(i = 0; i < ldA; i++)
    rowno[i] = i+1;
  
  lp = make_lp(ldA, p);
  if(lp == NULL)
    error("unable to create linear program");

  set_verbose(lp, 0);

  for(j = 0; j < p; j++) {
    set_columnex(lp, j+1, ldA, A+j*ldA, rowno);
    set_obj(lp, j+1, obj[j]);
  }    

  set_rh_vec(lp, b);

  for(i = 1; i < eq; i++)
    set_constr_type(lp, i, LE);

  for(i = eq; i <= ldA; i++)
    set_constr_type(lp, i, EQ);

  lpsInfinity = get_infinite(lp);
  for(j = 0; j < p; j++) {
    if(lb[j] == R_NegInf && ub[j] == R_PosInf)
      set_unbounded(lp, j+1);
    else {
      if(ub[j] == R_PosInf)
        ub[j] = lpsInfinity;
      else if(lb[j] == R_NegInf)
        lb[j] = -1.0 * lpsInfinity;
      set_bounds(lp, j+1, lb[j], ub[j]);
    }
  }

  for(j = 0; j < nInts; j++)
    set_int(lp, intvec[j], TRUE);

  if(pivotingRule >= 0)
    set_pivoting(lp, pivotingRule);

  if(simplexType >= 0)
    set_simplextype(lp, simplexType);

  if(basis[0] == 0) {
    check = set_basis(lp, basis, FALSE);
    if(check == FALSE)
      warning("user supplied basis was invalid - using phase 1 simplex");
  }

  if(epslevel >= 0) {
    set_epslevel(lp, epslevel);
  }
  else {
    if(eps[0] > 0.0)
      set_epsb(lp, eps[0]);
    if(eps[1] > 0.0)
      set_epsd(lp, eps[1]);
    if(eps[2] > 0.0)
      set_epsel(lp, eps[2]);
    if(eps[3] > 0.0)
      set_epsint(lp, eps[3]);
    if(eps[4] > 0.0)
      set_epsperturb(lp, eps[4]);
    if(eps[5] > 0.0)
      set_epspivot(lp, eps[5]);
  }

  if(presolve > 0)
    set_presolve(lp, presolve, get_presolveloops(lp));

  ret = solve(lp);

  eps[0] = get_epsb(lp);
  eps[1] = get_epsd(lp);
  eps[2] = get_epsel(lp);
  eps[3] = get_epsint(lp);
  eps[4] = get_epsperturb(lp);
  eps[5] = get_epspivot(lp);

  *pOptimalSolution = get_objective(lp);
  get_variables(lp, x);
  *pStatus = get_status(lp);

  delete_lp(lp);
}

