\name{nlminb2}


\alias{nlminb2}


\title{Constrained nonlinear minimization}
    
    
\description{

    Solve constrained nonlinear minimization 
    problem with nonlinear constraints using 
    a penalty and barrier approach.
    
}


\usage{
nlminb2(start, objective, eqFun = NULL, leqFun = NULL,  
    lower = -Inf, upper = Inf, gradient = NULL, hessian = NULL, 
    control = nlminb2Control(), env = .GlobalEnv)
}


\arguments{

    \item{start}{
        a numeric vector, initial values for the parameters to be 
        optimized.}
  
    \item{objective}{
        function to be minimized. Must return a scalar value (possibly 
        NA/Inf). The first argument to objective is the vector of 
        parameters to be optimized, whose initial values are supplied 
        through \code{start}. Further arguments (fixed during the course 
        of the optimization) to objective may be specified as well.
        see \code{env}. }
  
    \item{eqFun}{
        a list of functions describing equal constraints.}
  
    \item{leqFun}{
        a list of functions describing less equal constraints.}
  
    \item{lower, upper}{
        two vectors of lower and upper bounds, replicated to be as long 
        as \code{start}. If unspecified, all parameters are assumed to 
        be unconstrained.}
 
    \item{gradient}{
        an optional function that takes the same arguments as \code{objective} 
        and evaluates the gradient of \code{objective} at its first argument. 
        Must return a vector as long as \code{start}.}

    \item{hessian}{
        an optional function that takes the same arguments as \code{objective} 
        and evaluates the hessian of \code{objective} at its first argument. 
        Must return a square matrix of order \code{length(start)}. Only the 
        lower triangle is used.}
 
    \item{control}{
        a list of control parameters. See below for details.}

    \item{env}{
        the environment in which objective, constraint, control
        functions are evaluated.}
 
}


\value{

    A list with following elements:
  
    \item{par}{
        a numeric vector, the best set of parameters found.}
    
    \item{objective}{
        a numeric value, the value of \code{objective} corresponding 
        to \code{par}.}
  
    \item{convergence}{
        an integer code, 0 indicates successful convergence.}
  
    \item{message}{
        a character string giving any additional information returned 
        by the optimizer, or NULL. For details, see PORT documentation.}

    \item{iterations}{
        am integer value, the number of iterations performed.}

    \item{evaluations}{
        an integer value, the number of objective function and gradient 
        function evaluations.}
        
}


\author{ 
    For the R port of \code{nlminb} Douglas Bates and Deepayan Sarkar,
    for the R/Rmetrics port of \code{nlminbw} Diethelm Wuertz,
    for the PORT library netlib.bell-labs.com.}

\references{
    \url{http://netlib.bell-labs.com/netlib/port/}.
}


\seealso{
    \code{nlminb2Control},
    and packages \code{Rdonlp2} and \code{Rsolnp2}. 
}


\keyword{optimize}

