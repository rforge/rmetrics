\name{GpdDistribution}

\alias{GpdDistribution}

\alias{dgpd}
\alias{pgpd}
\alias{qgpd}
\alias{rgpd}

\alias{gpdMoments}
\alias{gpdSlider}


\title{Generalized Pareto Distribution}


\description{

    A collection and description of functions to compute
    the generalized Pareto distribution. The 
    functions compute density, distribution function, 
    quantile function and generate random deviates 
    for the GPD. In addition functions to 
    compute the true moments and to display the distribution
    and random variates changing parameters interactively 
    are available.
    \cr
    
    The GPD distribution functions are:
    
    \tabular{ll}{
    \code{dgpd} \tab Density of the GPD Distribution, \cr
    \code{pgpd} \tab Probability function of the GPD Distribution, \cr
    \code{qgpd} \tab Quantile function of the GPD Distribution, \cr
    \code{rgpd} \tab random variates from the GEV distribution, \cr
    \code{gpdMoments} \tab computes true mean and variance, \cr
    \code{gpdSlider} \tab displays density or rvs from a GPD.}
     
}


\usage{
dgpd(x, xi = 1, mu = 0, beta = 1, log = FALSE) 
pgpd(q, xi = 1, mu = 0, beta = 1, lower.tail = TRUE) 
qgpd(p, xi = 1, mu = 0, beta = 1, lower.tail = TRUE) 
rgpd(n, xi = 1, mu = 0, beta = 1)

gpdMoments(xi = 1, mu = 0, beta = 1)
gpdSlider(method = c("dist", "rvs"))
}


\arguments{
    
    \item{log}{
        a logical, if \code{TRUE}, the log density is returned.
        }
    \item{lower.tail}{
        a logical, if \code{TRUE}, the default, then
        probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
        }
    \item{method}{
        [gpdSlider] - \cr        
        a character string denoting what should be displayed. Either
        the density and \code{"dist"} or random variates \code{"rvs"}.
        } 
    \item{n}{
        [rgpd][gpdSim\ - \cr
        the number of observations to be generated.
        } 
    \item{p}{
        a vector of probability levels, the desired probability for the 
        quantile estimate (e.g. 0.99 for the 99th percentile).
        } 
    \item{q}{
        [pgpd] - \cr
        a numeric vector of quantiles.
        }
    \item{x}{
        [dgpd] - \cr
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
    \code{r*} generates random deviates.  
    
}


\author{

    Alec Stephenson for the functions from R's \code{evd} package, \cr
    Alec Stephenson for the functions from R's \code{evir} package, \cr
    Alexander McNeil for the EVIS functions underlying the \code{evir} package, \cr
    Diethelm Wuertz for this \R-port.
    
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, Springer. 
     
}


\examples{
## SOURCE("fExtremes.3A-GpdModelling")

## *gpd  -
   par(mfrow = c(2, 2))
   r = rgpd(n = 1000, xi = 1/4)
   plot(r, type = "l", main = "GPD Series")
   
## Plot empirical density and compare with true density:
## Omit values greater than 500 from plot
   hist(r, n = 50, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by = 0.01)
   lines(x, dgpd(x, xi = 1/4), col = "steelblue")
   
## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q = seq(-5, 5, by = 0.1)
   lines(q, pgpd(q, xi = 1/4), col = "steelblue")
   
## Compute quantiles, a test:
   qgpd(pgpd(seq(-1, 5, 0.25), xi = 1/4 ), xi = 1/4) 
}


\keyword{distribution}

