\name{sgh}

\alias{sgh}

\alias{dsgh}
\alias{psgh}
\alias{qsgh}
\alias{rsgh}


\title{Standardized Generalized Hyperbolic Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation for the standardized generalized
    hyperbolic distribution.
    
}


\usage{
dsgh(x, zeta = 1, rho = 0, lambda = 1, log = FALSE)
psgh(q, zeta = 1, rho = 0, lambda = 1)
qsgh(p, zeta = 1, rho = 0, lambda = 1)
rsgh(n, zeta = 1, rho = 0, lambda = 1)
}


\arguments{

    \item{zeta, rho, lambda}{
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

    All values for the \code{*sgh} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


\details{
  
    The generator \code{rsgh} is based on the GH algorithm given 
    by Scott (2004).
 
}


\author{

    Diethelm Wuertz.
    
}


\examples{   
## rsgh -
   set.seed(1953)
   r = rsgh(5000, zeta = 1, rho = 0.5, lambda = 1)
   plot(r, type = "l", col = "steelblue",
     main = "gh: zeta=1 rho=0.5 lambda=1")
 
## dsgh - 
   # Plot empirical density and compare with true density:
   hist(r, n = 50, probability = TRUE, border = "white", col = "steelblue",
     ylim = c(0, 0.6))
   x = seq(-5, 5, length = 501)
   lines(x, dsgh(x, zeta = 1, rho = 0.5, lambda = 1))
 
## psgh -  
   # Plot df and compare with true df:
   plot(sort(r), (1:5000/5000), main = "Probability", col = "steelblue")
   lines(x, psgh(x, zeta = 1, rho = 0.5, lambda = 1))
   
## qsgh -
   # Compute Quantiles:
   round(qsgh(psgh(seq(-5, 5, 1), zeta = 1, rho = 0.5), zeta = 1, rho = 0.5), 4)
}


\keyword{distribution}

