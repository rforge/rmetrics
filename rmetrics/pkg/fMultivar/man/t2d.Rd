\name{t2d}


\alias{t2d}
\alias{pt2d}
\alias{dt2d}
\alias{rt2d}


\title{Bivariate Student-t Distribution}


\description{
    
    Density, distribution function, and random 
    generation for the bivariate Student-t 
    distribution. 
    
}


\usage{
pt2d(x, y = x, rho = 0, nu = 4)
dt2d(x, y = x, rho = 0, nu = 4)
rt2d(n, rho = 0, nu = 4)
}


\arguments{

     \item{n}{
        the number of random deviates to be generated, an integer value.
        }
    \item{nu}{
        the number of degrees of freedom, a numeric value ranging between 
        two and infinity, by default four.      
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

    \code{pt2d}
    \cr
    returns a two column matrix of probabilities for the bivariate 
    Student-t distribution function.\cr
    
    \code{dt2d}
    \cr
    returns a two column matrix of densities for the bivariate 
    Student-t distribution function.\cr 
    
    \code{rt2d}
    \cr
    returns a two column matrix of random deviates generated from
    the bivariate Student-t distribution function.
    
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
## Bivariate Student-t Density:
   x = (-40:40)/10
   X = grid2d(x)
   z = dt2d(X$x, X$y, rho = 0.5, nu =6)
   Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
   persp(Z, theta = -40, phi = 30, col = "steelblue")
}


\keyword{math}

