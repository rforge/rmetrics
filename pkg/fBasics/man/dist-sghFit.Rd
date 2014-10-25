\name{sghFit}


\alias{sghFit}


\title{Standardized GH Distribution Fit}


\description{

    Estimates the distrinbutional parameters for a 
    standardized generalized hyperbolic distribution.

}


\usage{
sghFit(x, zeta = 1, rho = 0, lambda = 1, include.lambda = TRUE,
    scale = TRUE, doplot = TRUE, span = "auto", trace = TRUE, 
    title = NULL, description = NULL, \dots) 
}


\arguments{
  
    \item{x}{
        a numeric vector. 
        }
    \item{zeta, rho, lambda}{
        shape parameter \code{zeta} is positive,
        skewness parameter \code{rho} is in the range (-1, 1).
        and index parameter \code{lambda}, by default 1.
        }
    \item{include.lambda}{
        a logical flag, by default \code{TRUE}. Should the index
        parameter \code{lambda} included in the parameter estimate?
        }
    \item{scale}{
        a logical flag, by default \code{TRUE}. Should the time series
        be scaled by its standard deviation to achieve a more stable
        optimization?
        }
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }        
    \item{span}{
        x-coordinates for the plot, by default 100 values 
        automatically selected and ranging between the 0.001, 
        and 0.999 quantiles. Alternatively, you can specify
        the range by an expression like \code{span=seq(min, max,
        times = n)}, where, \code{min} and \code{max} are the 
        left and right endpoints of the range, and \code{n} gives 
        the number of the intermediate points.
        }        
    \item{trace}{
        a logical flag. Should the parameter estimation process be
        traced?
        }
    \item{title}{
        a character string which allows for a project title.
        }     
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{\dots}{
        parameters to be parsed.
        }

}


\value{
  
    returns a list with the following components:
  
    \item{estimate}{
        the point at which the maximum value of the log liklihood 
        function is obtained.
        }
    \item{minimum}{
        the value of the estimated maximum, i.e. the value of the
        log liklihood function.
        }
    \item{code}{
        an integer indicating why the optimization process terminated.\cr
        1: relative gradient is close to zero, current iterate is probably 
            solution; \cr
        2: successive iterates within tolerance, current iterate is probably 
            solution; \cr
        3: last global step failed to locate a point lower than \code{estimate}. 
            Either \code{estimate} is an approximate local minimum of the 
            function or \code{steptol} is too small; \cr
        4: iteration limit exceeded; \cr
        5: maximum step size \code{stepmax} exceeded five consecutive times. 
            Either the function is unbounded below, becomes asymptotic to a 
            finite value from above in some direction or \code{stepmax} 
            is too small.
            }
    \item{gradient}{
        the gradient at the estimated maximum.
        }
    \item{steps}{
        number of function calls.
        }
                     
}


\examples{    
## sghFit -
   # Simulate Random Variates:
   set.seed(1953)
   s = rsgh(n = 2000, zeta = 0.7, rho = 0.5, lambda = 0) 

## sghFit -  
   # Fit Parameters:
   sghFit(s, zeta = 1, rho = 0, lambda = 1, include.lambda = TRUE, 
     doplot = TRUE) 
}


\keyword{distribution}

