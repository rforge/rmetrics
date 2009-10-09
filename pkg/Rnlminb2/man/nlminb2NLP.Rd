\name{nlminb2NLP}


\alias{nlminb2NLP}


\title{Constrained nonlinear minimization}
    
    
\description{

    Solve constrained nonlinear minimization 
    problem with nonlinear constraints. An
    alternative function wrapper.
    
}


\usage{
nlminb2NLP( 
    par, fun,
    par.lower = NULL, par.upper = NULL,   
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,   
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    control = list())
}


\arguments{

    \item{par}{
        parameter vector(vector object).}
        
    \item{fun}{
        the objective function to be minimized. Currently, \code{fn}
        must take only one argument, and the parameter vector(\code{par})
        will be passed to \code{fn} during the optimization. The first
        element of return value must be the evaluated value.}
  
    \item{par.lower, par.upper}{
        upper and lower bounds for parameter vector, respectively. Their 
        length must equal to \code{length(par)}.}
  
    \item{eqA, ineqA}{
        the matrix objects that represents linear constraints. Its
        columns must be equal to \code{length(par)}, and its rows
        must be equal to the number of linear constraints.}
  
    \item{eqA.bound}{
        equality bounds for linear constraints, respectively. Their 
        length must equal to the number of linear constraints.}
    
    \item{ineqA.lower, ineqA.upper}{
        upper and lower bounds for linear constraints,
        respectively. Their length must equal to the number of linear
        constraints.}

    \item{eqFun}{
        list object whose elements are functions that represents
        nonlinear equality constraints.}
    
    \item{eqFun.bound}{
        equality bounds for nonlinear constraints, respectively.}
      
    \item{ineqFun}{
        list object whose elements are functions that represents
        nonlinear lower and upper constraints.}
 
    \item{ineqFun.lower, ineqFun.upper}{
        lower and upper bounds for nonlinear constraints, respectively.}

    \item{control}{
        list of control parameters that define the behaviour of the
        solver. See \code{nlminb2Control} for details.}
 
}


\value{

    For \code{n=length(par)} parameters, \code{lin} linear constraints,
    and \code{nlin} nonlinear constraints, a list with following elements:
  
    \item{par}{
        parameters returned by nlminb2NLP.}
    
    \item{gradf}{
        gradient evaluated at \code{par}.}
  
    \item{u}{
        \code{2*(n+lin+nlin)} vector of lagrange multipliers for
        constraints.}
  
    \item{w}{
        \code{2*(n+lin+nlin)} vector of  penalty term.}

    \item{step.nr}{
        total number of iterations.}

    \item{fx}{
        the value of objective function \code{fn}.}

    \item{sci}{
        scaling of \code{fn}.}

    \item{psi}{
        psi the weighted penalty term.}

    \item{upsi}{
        the unweighted penalty term(L1 norm of constraint
        vector).}

    \item{del.k.1}{
        bound for the last active constraints.}

    \item{b2n0}{
        weighted L2 norm of projected gradients.}

    \item{b2n}{
        L2norm of gradients based on \code{del.k.1}.}

    \item{nr}{
        number of binding constraints.}

    \item{sing}{
        value other than \code{-1} indicates working set is
        singular.}

    \item{umin}{
        infinity norm of negative part of inequalities
        multipliers.}

    \item{not.used}{always \code{0}(currently not used)}

    \item{cond.r}{
        condition number of diagonal part of qr decomposition of
        normalized gradients of binding constraints}

    \item{cond.h}{
        condition number of diagonal of cholesky factor
        of updated full Hessian.}

    \item{scf0}{
        the relative damping of tangential component if
        \code{upsi > tau0/2}.}
  
    \item{xnorm}{
        L2 norm of \code{par}.}

    \item{dnorm}{
        unsclaed norm of \code{d}, correction from eqp/qp
        subproblem.}

    \item{phase}{
        \code{-1}:infeasibility improvement phase, \code{0}:
        initial optimization, \code{1}:binding constraints unchanged,
        \code{2}:\code{d} small, maratos correction is in use.}

    \item{c.k}{
        number of decreases of penalty weights.}

    \item{wmax}{
        infinity norm of weights.}

    \item{sig.k}{
        stepsize from uidimensional minimization(backgracking).}

    \item{cfincr}{
        number of objective function evaluations for stepsize
        algorithm.}

    \item{dirder}{
        scaled derectional derivative of penalty function along
        \code{d}.}

    \item{dscal}{
        scaling factor for \code{d}.}

    \item{cosphi}{
        cosine of arc between \code{d} and previous \code{d}.}

    \item{violis}{
        number of constraints not binding at current values of
        \code{par} but hit during line search.}

    \item{hesstype}{
        one of 4 values indicating type of update for
        Hessian. \code{1}: normal P&M-BFGS update, \code{0}:update suppressed,
        \code{-1}:restart with scaled unit matrix, \code{2}:standard BFGS,
        \code{3}: BFGS modified by Powell's Device.}

    \item{modbifgs}{
        modification factor for damping the projector into the
        BFGS or pantoja-mayne update.}

    \item{modnr}{
        modification factor for daming the quasi-newton-relation
        in BFGS.}

    \item{qpterm}{
        \code{0}:if \code{sing==-1}, termination indicator of
        the QP solver, \code{1}:successful, \code{-1}:\code{\tau} becomes
        larger than \code{tauqp} without slack variables becoming sufficiently
        small.}

    \item{tauqp}{
        weight of slack variables in QP solver.}

    \item{infeas}{
        L1 norm of  slack variables in QP solver.}

    \item{nr.update}{
        the approximated newton-raphson update in upper
        trianglar form.}

    \item{hessian}{
        numeric Hessian matrix if \code{hessian=TRUE} in 
        \code{nlminb2NLPControl}.}

    \item{runtime}{
        the elapsed time for the optimization.}

    \item{message}{
        the termination message.}
}


\author{ 

    For the R port of \code{nlminb} Douglas Bates and Deepayan Sarkar,
    for the R/Rmetrics port of \code{nlminb2} Diethelm Wuertz,
    for the PORT library netlib.bell-labs.com.
    
}


\references{
Paul A. Jensen & Jonathan F. Bard, 
Operations Research Models and Methods, 2001
Appendix A, Algorithms for Constrained Optimization,
\url{http://www.me.utexas.edu/~jensen/ORMM/supplements/index.html}.

PORT Library,
\url{http://netlib.bell-labs.com/netlib/port/}.
}


\seealso{

    \code{nlminb2}, \code{nlminb2Control},
    and packages \code{Rdonlp2} and \code{Rsolnp2}. 
}


\examples{
## Example:

   # Feasible Start Solution:
   start = c(10, 10)
   
   # Objective Function: x^2 + y^2 
   fun <- function(x) sum(x^2)
   
   # Bounds: -100 <= x,y <= 100
   par.lower = c(-100, -100)
   par.upper = c(100, 100)
    
   # Equality Constraints: x*y = 2
   eqFun <- list(
     function(x) x[1]*x[2])
   eqFun.bound = 2
  
   # Solution: x = c(sqrt(2), sqrt(2)), f(x) = 4   
   nlminb2NLP(par = start, fun = fun, 
     par.lower = par.lower, par.upper = par.upper,
     eqFun = eqFun, eqFun.bound = eqFun.bound)[-1]
     
   # Solution: x = c(sqrt(2), sqrt(2)), f(x) = 4   
   nlminb2NLP(par = start, fun = fun, 
     par.lower = par.lower, par.upper = par.upper,
     eqFun = eqFun, eqFun.bound = eqFun.bound)[-1]
}


\keyword{optimize}

