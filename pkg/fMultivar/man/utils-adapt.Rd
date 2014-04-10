\name{utils-adapt}

\alias{adapt}


\title{Integrator for multivariate distributions}


\description{
    
    The function is for adaptive quadrature. 

}


\usage{
adapt(ndim, lower, upper, functn, \dots)
}


\note{

    In 2007 the package \code{adapt} was removed from the CRAN 
    repository, due to unclear license conditions. Nevertheless,
    formerly available versions can still be obtained from the CRAN 
    \href{http://cran.r-project.org/src/contrib/Archive/adapt/}{archive}.
    Package \code{adapt} used FORTRAN code from Professor Genz.
    
    From 2007 until 2013 the package \code{fMultivar} used an builtin
    licensed by Professor Genz to Rmetrics. This version is still 
    available in the current package, have a look into the folder 
    \code{deprecated}.
    
    2013 the contributed package \code{cubature} was added to the CRAN
    repository. This provides an alternative n-dimensional integration 
    routine. We recommend to use the function \code{adaptIntegrate} 
    directly from the package \code{cubature} which allows adaptive 
    multivariate integration over hypercubes. It is a wrapper around 
    the pure C, GPLed implementation by Steven G. Johnson.
    
    Since 2014 \code{fMultivar} uses also the C Version based 
    implementation of Johnson. The former function \code{adapt}
    has been replaced by a wrapper function calling \code{adaptIntegrate}.
    The arguments \code{ndim}, \code{lower}, \code{upper}, and
    \code{functn} have been remeined the same, control parameters have
    been adapted to the function \code{cubature::adaptIntegrate}.
    
  
}


\arguments{

    \item{ndim}{   
        the dimension of the integral. By default NUll, no longer
        used.
        }
    \item{lower}{
        vector of at least length \code{ndim} of the lower bounds 
        on the integral.
        }
    \item{upper}{	
        vector of at least length \code{ndim} of the upper bounds 
        on the integral.
        }
    \item{functn}{	
        an R function which should take a single vector argument and 
        possibly some parameters and return the function value at 
        that point. \code{functn} must return a single numeric value.
        }
    \item{\dots}{	
        other parameters to be passed to the underlying function.
        }
        
}


\value{

  The returned value is a list of three items:

  \item{integral}{   
      the value of the integral.
      }
  \item{error}{ 
      the estimated relative error.
      }
  \item{functionEvaluations}{ 
      the number of times the function was evaluated.
      }
  \item{returnCode}{ 
      the actual integer return code of the C routine.
      }


}


\references{
    See: http://ab-initio.mit.edu/wiki/index.php/Cubature.
}


\author{
    Balasubramanian Narasimhan
}


\examples{
## Check that dnorm2d is normalized:
  
   # Normal Density:
   density <- function(x) dnorm2d(x=x[1], y = x[2])
      
   # Calling Cubature:
   BIG <- c(99, 99)
   cubature::adaptIntegrate(f=density, lowerLimit=-BIG, upperLimit=BIG)
   cubature::adaptIntegrate(f=density, low=-BIG, upp=BIG, tol=1e-7)
    
   # Using the Wrapper:
   adapt(lower=-BIG, upper=BIG, functn=density) 
   adapt(lower=-BIG, upper=BIG, functn=density, tol=1e-7)$integral   
}


\keyword{math}


