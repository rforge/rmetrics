\name{GpdDistribution}

\alias{GpdDistribution}

\alias{dgpd}
\alias{pgpd}
\alias{qgpd}
\alias{rgpd}

\title{GPD Distributions for Extreme Value Theory}


\description{

    A collection and description of distribution functions 
    used in extreme value theory. The functions compute 
    density, distribution function, quantile function and 
    generate random deviates for the Generalized Pareto 
    Distribution GPD.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{dgpd} \tab Density of the GPD Distribution, \cr
    \code{pgpd} \tab Probability function of the GPD Distribution, \cr
    \code{qgpd} \tab Quantile function of the GPD Distribution, \cr
    \code{rgpd} \tab Random variates from the GPD Distribution. }
     
}


\usage{
dgpd(x, xi = 1, mu = 0, beta = 1) 
pgpd(q, xi = 1, mu = 0, beta = 1) 
qgpd(p, xi = 1, mu = 0, beta = 1) 
rgpd(n, xi = 1, mu = 0, beta = 1)
}


\arguments{

    \item{n}{
        the number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{q}{
        a numeric vector of quantiles.
        }
    \item{x}{
        a numeric vector of quantiles.
        }
    \item{xi, mu, beta}{
        \code{xi} is the shape parameter, 
        \code{mu} the location parameter,
        and \code{beta} is the scale parameter.
        }

}


\value{
    
    All values are numeric vectors: \cr
    \code{d*} returns the density, \cr
    \code{p*} returns the probability, \cr
    \code{q*} returns the quantiles, and \cr
    \code{r*} generates random deviates. \cr
}


\details{
    
    \bold{Generalized Pareto Distribution:}
    \cr\cr
    Compute density, distribution function, quantile function and 
    generates random variates for the Generalized Pareto Distribution.
        
}


\author{

    Alec Stephenson for the functions from R's \code{evd} package, \cr
    Diethelm Wuertz for this \R-port.
    
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer. 
     
}


\examples{
## SOURCE("fExtremes.053A-GpdModelling")

## *gpd  -
   xmpExtremes("\nStart: Simulate GPD Distributed sample >")
   par(mfrow = c(2, 2))
   r = rgpd(n = 1000, xi = 1/4)
   plot(r, type = "l", main = "GPD Series")
   
## Plot empirical density and compare with true density:
## Omit values greater than 500 from plot
   xmpExtremes("\nNext: Plot Empirical and True Density >")
   hist(r, n = 50, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by = 0.01)
   lines(x, dgpd(x, xi = 1/4), col = "steelblue3")
   
## Plot df and compare with true df:
   xmpExtremes("\nNext: Plot Empirical and True Probability >")
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q = seq(-5, 5, by = 0.1)
   lines(q, pgpd(q, xi = 1/4), col = "steelblue3")
   
## Compute quantiles, a test:
   xmpExtremes("\nNext: Compute Quantiles >")
   qgpd(pgpd(seq(-1, 5, 0.25), xi = 1/4 ), xi = 1/4) 
 
}


\keyword{distribution}

