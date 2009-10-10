\name{solnp2}


\alias{solnp2}


\title{Nonlinear optimization using augmented lagrange method.}


\description{

    The solnp function is based on the solver by Yinyu Ye which solves 
    the general nonlinear programming problem:\cr

    minimize        \eqn{f(x)}\cr 

    subject to      \eqn{g(x) = 0}\cr
            \eqn{l_h \le h(x) \le u_h}\cr
            \eqn{l_x \le x \le u_x}\cr

    where, \eqn{f(x)}, \eqn{g(x)} and \eqn{g(x)} are smooth functions.
}


\usage{
solnp2(pars, fun, grad = NULL, eqfun = NULL, eqB = NULL, eqgrad = NULL, 
    ineqfun = NULL, ineqLB = NULL, ineqUB = NULL, ineqgrad = NULL, 
    LB = NULL, UB = NULL, control = list(), env = ,GlobalEnv)
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
        upper and lower bounds for parameter vector,
        respectively. Their length must equal to
        \code{length(par)}.}
  
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
        solver. See \code{\link{solnp2Control}} for details.}
}


\details{

    The solver belongs to the class of indirect solvers and implements 
    the augmented Lagrange multiplier method with an SQP interior algorithm.

    }


\value{

    A list containing the following values:
    
    \item{pars}{
        Optimal Parameters.}
        
    \item{convergence }{
        Indicates whether the solver has converged (0) or not (1).}
        
    \item{values}{
        Vector of function values during optimization with last one the
        value at the optimal.}
        
    \item{lagrange}{
        The vector of Lagrange multipliers.}
        
    \item{hessian}{
        The Hessian at the optimal solution.}
        
    \item{ineqx0}{
        The estimated optimal inequality vector of slack variables used 
        for transforming the inequality into an equality constraint.}
        
    \item{nfuneval}{The number of function evaluations.}
    
    \item{elapsed}{Time taken to compute solution.}
    
}


\section{Control}{
    
    \describe{
    
        \item{rho}{
            Penalty parameter (default 1).}
            
        \item{outer.iter}{
            Maximum number of major (outer) iterations (default 400).}
            
        \item{inner.iter}{
            Maximum number of minor (inner) iterations (default 800).}
            
        \item{delta}{
            Relative step size in forward difference evaluation (default 
            1.0e-8).}
            
        \item{tol}{
            Tolerance on feasibility and optimality (default 1e-6).}
            
        \item{trace}{
            The value of the objective function and the parameters is 
            printed at every major iteration (default 0).}
        
    }
    
}


\references{
    
    Y.Ye,\emph{Interior algorithms for linear, quadratic, and linearly  
        constrained non linear programming}, PhD Thesis, Department of 
        EES Stanford University, Stanford CA.
}


\author{

    Alexios Ghalanos and Stefan Theussl\cr
    Y.Ye (original matlab version of solnp)
    
}


\note{

    The control parameters \code{tol} and \code{delta} are key in getting 
    any possibility of successful convergence, therefore it is suggested 
    that the user change these appropriately to reflect their problem 
    specification. 
    
    The solver is a local solver, therefore for problems with rough surfaces 
    and many local minima there is absolutely no reason to expect anything 
    other than a local solution.
    
}


\examples{
## Powell Problem - 
   # From the original paper by Y.Ye
   # see the unit tests for more....
   fn1 = function(x) {
     exp(x[1]*x[2]*x[3]*x[4]*x[5]) }

## Equality Constraints - 
   eqn1 <- function(x){
     z1 = x[1]*x[1]+x[2]*x[2]+x[3]*x[3]+x[4]*x[4]+x[5]*x[5]
     z2 = x[2]*x[3]-5*x[4]*x[5]
     z3 = x[1]*x[1]*x[1]+x[2]*x[2]*x[2]
     return(c(z1,z2,z3)) }

## Start Values -
   x0 = c(-2, 2, 2, -1, -1)
    
## Solve - 
   powell = solnp2(x0, fun = fn1, eqfun = eqn1, eqB = c(10, 0, -1))
}


\keyword{optimize}

