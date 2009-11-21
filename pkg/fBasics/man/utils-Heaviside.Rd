\name{Heaviside}


\alias{Heaviside}
\alias{Sign}
\alias{Delta}
\alias{Boxcar}
\alias{Ramp}


\title{Haviside and Related Functions}


\description{

    Functions which compute the Heaviside and 
    related functions. These include the sign 
    function, the delta function, the boxcar 
    function, and the ramp function.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{Heaviside} \tab Computes Heaviside unit step function, \cr
    \code{Sign} \tab Just another signum function, \cr
    \code{Delta} \tab Computes delta function, \cr
    \code{Boxcar} \tab Computes boxcar function, \cr
    \code{Ramp} \tab Computes ramp function. }
            
}


\usage{
Heaviside(x, a = 0)
Sign(x, a = 0)
Delta(x, a = 0)
Boxcar(x, a = 0.5)
Ramp(x, a = 0)
}


\arguments{
     
    \item{a}{
        a numeric value, the location of the break.\cr
        }
    \item{x}{
        a numeric vector.
        }
}


\details{
  
    The Heaviside step function \code{Heaviside} is 1 for \code{x>a}, 
    \code{1/2} for \code{x=a}, and \code{0} for \code{x<a}.
    \cr
    
    The Sign function \code{Sign} is \code{1} for \code{x>a}, 
    \code{0} for \code{x=a}, and \code{-1} for \code{x<a}.
    \cr
    
    The delta function \code{Delta} is defined as: 
    \code{Delta(x) = d/dx H(x-a)}.
    \cr
    
    The boxcar function \code{Boxcar} is defined as: 
    \code{Boxcar(x) = H(x+a) - H(x-a)}.
    \cr
    
    The ramp function is defined as: 
    \code{Ramp(x)= (x-a)*H(x-a)}.
    
}


\value{

    returns the function values of the selected function.
    
}


\note{

    The Heaviside function is used in the implementation of the skew
    Normal, Student-t, and Generalized Error distributions, distributions
    functions which play an important role in modelling GARCH processes.
    
}


\seealso{
  
    \code{GarchDistribution},
    \code{GarchDistributionFits}.
    
}


\references{

Weisstein W. (2004);
    \emph{http://mathworld.wolfram.com/HeavisideStepFunction.html},
    Mathworld.
    
}


\examples{
## Heaviside -
   x = sort(round(c(-1, -0.5, 0, 0.5, 1, 5*rnorm(5)), 2))
   h = Heaviside(x)
    
## Sign -
   s = Sign(x)
   
## Delta -
   d = Delta(x)
   
## Boxcar -
   Pi = Boxcar(x)

## Ramp - 
   r = Ramp(x)
   cbind(x = x, Step = h, Signum = s, Delta = d, Pi = Pi, R = r)        
}


\keyword{programming}

