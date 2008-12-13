\name{snig}


\alias{snig}

\alias{dsnig}
\alias{psnig}
\alias{qsnig}
\alias{rsnig}


\title{Standardized Normal Inverse Gaussian Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation for the standardized normal 
    inverse Gaussian distribution.
    
}


\usage{
dsnig(x, zeta = 1, rho = 0, log = FALSE)
psnig(q, zeta = 1, rho = 0)
qsnig(p, zeta = 1, rho = 0)
rsnig(n, zeta = 1, rho = 0)
}


\arguments{
 
    \item{zeta, rho}{
        shape parameter \code{zeta} is positive,
        skewness parameter \code{rho} is in the range (-1, 1).
        }
    \item{log}{
        a logical flag by default \code{FALSE}. 
        If TRUE, log values are returned.
        }
    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    
}


\value{

    All values for the \code{*snig} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


\details{
  
    The random deviates are calculated with the method described by 
    Raible (2000).
    
}


\author{

    Diethelm Wuertz.
    
}


\examples{   
## snig -
   set.seed(1953)
   r = rsnig(5000, zeta = 1, rho = 0.5)
   plot(r, type = "l", col = "steelblue",
     main = "snig: zeta=1 rho=0.5")
 
## snig - 
   # Plot empirical density and compare with true density:
   hist(r, n = 50, probability = TRUE, border = "white", col = "steelblue")
   x = seq(-5, 5, length = 501)
   lines(x, dsnig(x, zeta = 1, rho = 0.5))
 
## snig -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, psnig(x, zeta = 1, rho = 0.5))
   
## snig -
   # Compute Quantiles:
   qsnig(psnig(seq(-5, 5, 1), zeta = 1, rho = 0.5), zeta = 1, rho = 0.5) 
}


\keyword{distribution}

