\name{utils-adapt}

\alias{adapt}


\title{Integrator for multivariate distributions}


\description{
    
    The function is for adaptive quadrature. The underlyling 
    fortran code is purported to work in from 2 to 20 dimensions.   

}


\usage{
adapt(ndim, lower, upper, minpts = 100, maxpts = NULL, functn, 
    eps = 0.01, \dots)
}


\details{

    The function computes computes an n-dimensional integral between
    lower and upper bounds. Infinite rectangles are not allowed, and 
    the number of dimensionas must be between 2 and 20.

    The function is modified from Mike Meyer's S code. The functions 
    just calls A.C. Genz's fortran ADAPT subroutine to do all of the 
    calculations. A work array is allocated within the C/Fortran code.

    The Fortran function has been modified to use double precision, 
    for compatibility with R. It only works in two or more dimensions; 
    for one-dimensional integrals use the \code{integrate} function 
    in the base package.

    Setting \code{maxpts} to NULL asks the function to keep doubling 
    \code{maxpts} (starting at \code{max(minpts,500, r(ndim))}) until 
    the desired precision is achieved or R runs out of memory. Note 
    that the necessary number of evaluations typically grows 
    exponentially with the dimension \code{ndim}, and the underlying 
    code requires \code{maxpts >= r(ndim)} where \code{r(d) = 2^d + 
    2 d(d + 3) + 1}.
  
}


\arguments{

    \item{ndim}{   
        the dimension of the integral.
        }
    \item{lower}{
        vector of at least length \code{ndim} of the lower bounds 
        on the integral.
        }
    \item{upper}{	
        vector of at least length \code{ndim} of the upper bounds 
        on the integral.
        }
    \item{minpts}{
        the minimum number of function evaluations.
        }
    \item{maxpts}{	
        the maximum number of function evaluations or NULL per 
        default, see Details.
        }
    \item{functn}{	
        an R function which should take a single vector argument and 
        possibly some parameters and return the function value at 
        that point. \code{functn} must return a single numeric value.
        }
    \item{eps}{	
        the desired accuracy for the relative error.
        }
    \item{\dots}{	
        other parameters to be passed to \code{functn}.
        }
        
}


\value{
    
    A list of \code{class} \code{"integration"} with components

    \item{value}{  
        the estimated integral.
        }
    \item{relerr}{ 	
        the estimated relative error; < \code{eps} argument if the 
        algorithm converged properly.
        }
    \item{minpts}{
        the actual number of function evaluations.
        }

    \item{ifail}{
        an error indicator. If \code{ifail} is not equal to 0, the function 
        warns the user of the error condition.
        }
    
}


%\references{}


%\author{}


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

