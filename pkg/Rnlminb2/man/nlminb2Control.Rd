\name{nlminb2Control}

\alias{nlminb2Control}

\title{Control variables for Rnlminb2}


\description{
Collection of Control Variables}


\usage{
nlminb2Control(eval.max = 500, iter.max = 400, trace = 0, abs.tol = 1e-20, 
    rel.tol = 1e-10, x.tol = 1.5e-8, step.min = 2.2e-14, scale = 1,
    R = 1.0, beta = 0.01, steps.max = 10, steps.tol = 1e-6)
}


\arguments{
    \item{eval.max}{
        an integer value. Maximum number of evaluations of the objective 
        function allowed. Defaults to 500.}
  
    \item{iter.max}{
        an integer value. Maximum number of iterations allowed. Defaults 
        to 400.}

    \item{trace}{
        an integer value. The value of the objective function and the 
        parameters is printed every trace'th iteration. Defaults to 0 
        which indicates no trace information is to be printed.}
        
    \item{abs.tol}{
        a numeric value. Absolute tolerance. Defaults to 1e-20.}

    \item{rel.tol}{
        a numeric value. Relative tolerance. Defaults to 1e-10.}

    \item{x.tol}{
        a numeric value. X tolerance. Defaults to 1.5e-8.}

    \item{step.min}{
        a numeric value. Minimum step size. Defaults to 2.2e-14.}
        
    \item{scale}{
        See PORT documentation (or leave alone).}

    \item{R}{
        a numeric value. The multiplier and devisor for the barrier and
        penalty function terms. Defaults to 1.0}

    \item{beta}{
        a numeric value. The value by which R is lowered in each iteration 
        step. Defaults to 0.01.}

    \item{steps.max}{
        an integer value. The maximum number of iteration steps in 
        which the penalty and barrier terms are lowered. Defaults to 10.}

    \item{steps.tol}{
        a numeric value. The penalty and barrier tolerance. Defaults
        to 1e-6.}

}


\seealso{
    \code{nlminb2}, \code{nlminb2NLP}, and \code{nlminb2Control}.
}


\keyword{optimize}

