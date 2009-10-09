\name{donlp2NLP}


\alias{donlp2NLP}


\title{Solve constrained nonlinear minimization problem}
    

\description{

    Solve constrained nonlinear minimization problem.
    An alternative R interface
    
}


\details{
    
    An alternative interface which may be suite better for portfolio
    optimization compared with the default interface function 
    \code{donlp2}.

}
    

\usage{
donlp2NLP( 
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
        solver. See \code{\link{donlp2Control}} for details.}
}


\value{

    A list with following elements:
  
    \item{opt}{
        a list of information on the optimal solution as returned by
        the function \code{donlp2}.}
    
    \item{par}{
        a numeric vector, the optimal solution.}
  
    \item{objective}{
        a numeric value, the value at the optimal solution}
  
    \item{convergence}{
        not used, returns NA.}

}


\author{ 

    The R port of \code{dnonlp2} was written by Ryuichi Tamura,
    the R/Rmetrics interface \code{donlp2NLP} was written by Diethem Wuertz,  
    the underlying C code called by the R function \code{donlp2} was 
    written by Peter Sperucci.
    
}

\references{

    \url{http://plato.la.asu.edu/donlp2NLP.html}(original donlp2NLP),
    \url{http://arumat.net/Rdonlp2NLP/}(Rdonlp2NLP)
    
}


\seealso{

    \code{\link{donlp2Control}},
    and packages \code{Rnlminb2} and \code{Rsolnp2}.
    
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
   donlp2NLP(par = start, fun = fun, 
     par.lower = par.lower, par.upper = par.upper,
     eqFun = eqFun, eqFun.bound = eqFun.bound)[-1]
}


\keyword{optimize}

