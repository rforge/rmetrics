\name{adapt}

\title{Adaptive Numerical Integration in 2--20 Dimensions}


\alias{adapt}
\alias{print.integration}


\usage{
adapt(ndim, lower, upper, minpts = 100, maxpts = NULL, functn, 
    eps = 0.01, \dots)
}


\description{

    Integrates a scalar function over a multidimensional
    rectangle. 
 
}
   
    
\details{

    The function computes
    
        \deqn{\int_l^u \mbox{functn}(t)\,d^nt}{%
            integral[l .. u] functn(t) d^n(t)}
        
    where \eqn{l =}\code{lower}, \eqn{u =}\code{upper} and \eqn{n =}\code{ndim}.
    Infinite rectangles are not allowed, and \code{ndim} must be between 2 
    and 20.
    
    This is modified from Mike Meyer's S code. The functions just
    call A.C. Genz's fortran ADAPT subroutine to do all of the
    calculations.  A work array is allocated within the C/Fortran code.

    The Fortran function has been modified to use double precision, for
    compatibility with \R. It only works in two or more dimensions; for
    one-dimensional integrals use the \code{\link{integrate}} function 
    in the base package.

    Setting \code{maxpts} to NULL asks the function to keep doubling
    \code{maxpts} (starting at \code{max(minpts,500, r(ndim))}) until 
    the desired precision is achieved or \R runs out of memory. Note that 
    the necessary number of evaluations typically grows exponentially 
    with the dimension \code{ndim}, and the underlying code requires 
    \code{maxpts >= r(ndim)} where \eqn{r(d) = 2^d + 2 d(d + 3) + 1}.
}


\arguments{

    \item{ndim}{
        the dimension of the integral, andi.e. number}
    \item{lower}{
        vector of at least length \code{ndim} of the lower bounds
        on the integral.}
    \item{upper}{
        vector of at least length \code{ndim} of the upper bounds
        on the integral.}
    \item{minpts}{
        the minimum number of function evaluations.}
    \item{maxpts}{
        the maximum number of function evaluations or
        \code{NULL} per default, see \emph{Details}.}
    \item{functn}{
        an \R function which should take a single vector
        argument and possibly some parameters and return the function value
        at that point.  \code{functn} must return a single numeric value.}
    \item{eps}{
        the desired accuracy for the relative error.}
    \item{\dots}{
        other parameters to be passed to \code{functn}}
}


\value{
  
    A list of \code{\link{class} "integration"} with components
  
    \item{value}{
        the estimated integral}
    \item{relerr}{
        the estimated relative error; \code{< eps} argument if
        the algorithm converged properly.}
    \item{minpts}{
        the actual number of function evaluations}
    \item{ifail}{
        an error indicator.  If \code{ifail} is not equal to 0,
        the function warns the user of the error condition.}
}


\seealso{

    \code{\link{integrate}}
    
}


\examples{
## Example of  p - dimensional spherical normal distribution:
   ir2pi <- 1/sqrt(2*pi)
   fred <- function(z) { ir2pi^length(z) * exp(-0.5 * sum(z * z))}

   adapt(2, lo = c(-5, -5), up = c(5, 5), functn = fred)
   adapt(2, lo = c(-5, -5), up = c(5, 5), functn = fred, eps = 1e-4)
   adapt(2, lo = c(-5, -5), up = c(5, 5), functn = fred, eps = 1e-6)

## adapt "sees" function ~= constantly 0 --> wrong result
   adapt(2, lo = c(-9,-9), up = c(9,9), functn = fred)

## fix by using much finer initial grid:
   adapt(2, lo = c(-9,-9), up = c(9,9), functn = fred, min = 1000)
   adapt(2, lo = c(-9,-9), up = c(9,9), functn = fred, min = 1000, eps = 1e-6)
   i1 <- print(integrate(dnorm, -2, 2))$value

## True values for the following example:
   i1 ^ c(3, 5)
   for(p in c(3, 5)) {
     cat("\np = ", p, "\n------\n")
     f.lo <- rep(-2., p)
     f.up <- rep(+2., p)
     # not enough evaluations:
     print(adapt(p, lo=f.lo, up=f.up, max=100*p, functn = fred))
     # enough evaluations:
     print(adapt(p, lo=f.lo, up=f.up, max=10^p,  functn = fred))
     # no upper limit; p=3: 7465 points, ie 5 attempts (on an Athlon/gcc/g77):
     print(adapt(p, lo=f.lo, up=f.up, functn = fred, eps = 1e-5))
  }
}


\keyword{math}


\keyword{utilities}

