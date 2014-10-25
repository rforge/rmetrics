\name{std}


\alias{std}
\alias{dstd}
\alias{pstd}
\alias{qstd}
\alias{rstd}


\title{Student-t Distribution}


\description{
    
    Functions to compute density, distribution function, 
    quantile function and to generate random variates 
    for the Student-t distribution.
       
}


\usage{
dstd(x, mean = 0, sd = 1, nu = 5, log = FALSE)
pstd(q, mean = 0, sd = 1, nu = 5)
qstd(p, mean = 0, sd = 1, nu = 5)
rstd(n, mean = 0, sd = 1, nu = 5)
}


\arguments{

    \item{mean, sd, nu}{
        location parameter \code{mean},
        scale parameter \code{sd},
        shape parameter \code{nu}.
        }
    \item{n}{
        the number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    \item{log}{
        a logical; if TRUE, densities are given as log densities.
        }
}


\value{

    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates, \cr
    all values are numeric vectors.
 
}


\references{
   
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## std -
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rstd(n = 1000)
   plot(r, type = "l", main = "sstd", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dstd(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, pstd(x), lwd = 2)
   
   # Compute quantiles:
   round(qstd(pstd(q = seq(-1, 5, by = 1))), digits = 6)
}


\keyword{distribution}

