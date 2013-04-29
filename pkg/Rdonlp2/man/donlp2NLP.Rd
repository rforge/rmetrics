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
    start, fun,
    par.lower = NULL, par.upper = NULL,   
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,  
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL, 
    control = list())
}


\arguments{

    \item{start}{
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
    
    \item{solution}{
        a numeric vector, the optimal solution.}
  
    \item{objective}{
        a numeric value, the value at the optimal solution}
  
    \item{status}{
        not used, returns NA.}
        
    \item{message}{
        a character string, the convergence message.}
        
    \item{solver}{
        a character string, the name of the solver.}
        
    \item{version}{
        a character string, the convergence message.}

}


\author{ 

    Peter Speluccihas has written the original solver code,
    S. Schoeffert has translated donlp2 from f77 to the ANSI C version,
    K. S. Cove has added dynamic memory allocation,
    Christoph Bergmeier has added passing objecive and constraints as external pointer,
    Ryuichi Tamura has written the original Rdonlp2 interface,
    Diethelm Wuertz has written the current Rdonlp2 interface.
    
}



\examples{
## Example:

   # Feasible Start Solution:
   start <- c(10, 10)
   
   # Objective Function: x^2 + y^2 
   fun <- function(x) sum(x^2)
   
   # Bounds: -100 <= x,y <= 100
   par.lower <- c(-100, -100)
   par.upper <- c(100, 100)
    
   # Equality Constraints: x*y = 2
   eqFun <- list(function(x) x[1]*x[2])
   eqFun.bound <- 2
  
   # Solution: x = c(sqrt(2), sqrt(2)), f(x) = 4   
   donlp2NLP(start = start, fun = fun, 
     par.lower = par.lower, par.upper = par.upper,
     eqFun = eqFun, eqFun.bound = eqFun.bound)[-1]
}


\keyword{optimize}

