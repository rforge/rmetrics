\name{norm2d}

\alias{norm2d}

\alias{pnorm2d}
\alias{dnorm2d}
\alias{rnorm2d}


\title{Bivariate Normal Distribution}


\description{
    
    Density, distribution function, and random 
    generation for the bivariate normal
    distribution.
}


\usage{
pnorm2d(x, y = x, rho = 0)
dnorm2d(x, y = x, rho = 0)
rnorm2d(n, rho = 0)
}


\arguments{

     \item{n}{
        the number of random deviates to be generated, an integer value.
        }
    \item{rho}{
        the correlation parameter, a numeric value ranging between 
        minus one and one, by default zero.     
        }
    \item{x, y}{
        two numeric vectors defining the x and y coordinates. 
        }    
        
}


\value{

    \code{pnorm2d}
    \cr
    returns a two column matrix of probabilities for the bivariate 
    normal distribution function.\cr
    
    \code{dnorm2d}
    \cr
    returns a two column matrix of densities for the bivariate 
    normal distribution function.\cr 
    
    \code{rnorm2d}
    \cr
    returns a two column matrix of random deviates generated from
    the bivariate normal distribution function.
    
}


\references{
    
Azzalini A., (2004);
    \emph{The sn Package};
    R Reference Guide available from www.r-project.org.
    
Venables W.N., Ripley B.D., (2002); 
    \emph{Modern Applied Statistics with S},
    Fourth Edition, Springer.
            
}


\author{

    Adelchi Azzalini for the underlying pnorm2d function, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Bivariate Normal Density:
   x = (-40:40)/10
   X = grid2d(x)
   z = dnorm2d(X$x, X$y, rho = 0.5)
   Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
   persp(Z, theta = -40, phi = 30, col = "steelblue")
}


\keyword{math}

