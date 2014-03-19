\name{bvdist-norm2d}

\alias{norm2d}

\alias{dnorm2d}
\alias{pnorm2d}
\alias{rnorm2d}


\title{Bivariate Normal Distribution}


\description{
    
    Density, distribution function, and random 
    generation for the bivariate normal
    distribution.
}


\usage{
dnorm2d(x, y, rho = 0)
pnorm2d(x, y, rho = 0)
rnorm2d(n, rho = 0)
}


\arguments{

     \item{x, y}{
        two numeric vectors defining the x and y coordinates. 
        }   
    \item{n}{
        the number of random deviates to be generated, an integer value.
        }
    \item{rho}{
        the correlation parameter, a numeric value ranging between 
        minus one and one, by default zero.     
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
## dnorm2d -
   # Bivariate Normal Density:
   x <- (-40:40)/10
   X <- grid2d(x)
   z <- dnorm2d(X$x, X$y, rho = 0.5)
   ZD <- list(x = x, y = x, z = matrix(z, ncol = length(x)))
   # Perspective Plot:
   persp(ZD, theta = -40, phi = 30, col = "steelblue")
   # Contour Plot:
   contour(ZD)
    
## pnorm2d -
   # Bivariate Normal Probability:
   z <- pnorm2d(X$x, X$y, rho = 0.5)
   ZP <- list(x = x, y = x, z = matrix(z, ncol = length(x)))
   # Perspective Plot:
   persp(ZP, theta = -40, phi = 30, col = "steelblue")
   # Contour Plot:
   contour(ZP)
   
## rnorm2d - 
   # Bivariate Normal Random Deviates
   r <- rnorm2d(5000, rho=0.5)
   # Scatter Plot:
   plot(r, col="steelblue", pch=19, cex=0.5) 
   contour(ZD, add=TRUE, lwd=2, col="red")
   # Hexagonal Binning:
   plot(hexBinning(r))
   contour(ZD, add=TRUE, lwd=2, col="black")
}


\keyword{math}

