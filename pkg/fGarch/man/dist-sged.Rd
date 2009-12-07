\name{sged}

\alias{sged}

\alias{dsged}
\alias{psged}
\alias{qsged}
\alias{rsged}

\alias{gedFit}
\alias{sgedFit}

\alias{sgedSlider}


\title{Skew Generalized Error Distribution}


\description{
    
    Functions to compute density, distribution function, 
    quantile function and to generate random variates 
    for the skew generalized error distribution.
          
}


\usage{
dsged(x, mean = 0, sd = 1, nu = 2, xi = 1.5, log = FALSE)
psged(q, mean = 0, sd = 1, nu = 2, xi = 1.5)
qsged(p, mean = 0, sd = 1, nu = 2, xi = 1.5)
rsged(n, mean = 0, sd = 1, nu = 2, xi = 1.5)
}


\arguments{

    \item{mean, sd, nu, xi}{
        location parameter \code{mean},
        scale parameter \code{sd},
        shape parameter \code{nu}, 
        skewness parameter \code{xi}.
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

Nelson D.B. (1991);
    \emph{Conditional Heteroscedasticity in Asset Returns: A New Approach},
    Econometrica, 59, 347--370.
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## sged -
   par(mfrow = c(2, 2))
   set.seed(1953)
   r = rsged(n = 1000)
   plot(r, type = "l", main = "sged", col = "steelblue")
   
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue")
   box()
   x = seq(min(r), max(r), length = 201)
   lines(x, dsged(x), lwd = 2)
   
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue",
     ylab = "Probability")
   lines(x, psged(x), lwd = 2)
   
   # Compute quantiles:
   round(qsged(psged(q = seq(-1, 5, by = 1))), digits = 6)
}


\keyword{distribution}

