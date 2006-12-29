\name{GevDistribution}

\alias{GevDistribution}

\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}

\alias{gevMoments}
\alias{gevSlider}


\title{Generalized Extreme Value Distribution}


\description{
  
    A collection and description of functions to compute
    the generalized extreme value distribution. The 
    functions compute density, distribution function, 
    quantile function and generate random deviates 
    for the GEV including the Frechet, Gumbel, and 
    Weibull distributions. In addition functions to 
    compute the true moments and to display the distribution
    and random variates changing parameters interactively 
    are available.
    \cr
    
    The GEV distribution functions are:
    
    \tabular{ll}{
    \code{dgev} \tab density of the GEV distribution, \cr
    \code{pgev} \tab probability function of the GEV distribution, \cr
    \code{qgev} \tab quantile function of the GEV distribution, \cr
    \code{rgev} \tab random variates from the GEV distribution, \cr
    \code{gevMoments} \tab computes true mean and variance, \cr
    \code{gevSlider} \tab displays density or rvs from a GEV.}
    
}


\usage{
dgev(x, xi = 1, mu = 0, beta = 1, log = FALSE)
pgev(q, xi = 1, mu = 0, beta = 1, lower.tail = TRUE)
qgev(p, xi = 1, mu = 0, beta = 1, lower.tail = TRUE)
rgev(n, xi = 1, mu = 0, beta = 1)

gevMoments(xi = 0, mu = 0, beta = 1)
gevSlider(method = c("dist", "rvs"))
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
        [gevSlider] - \cr        
        a character sgtring denoting what should be displayed. Either
        the density and \code{"dist"} or random variates \code{"rvs"}.
        } 
    \item{n}{
        [rgev] - \cr        
        the number of observations.
        } 
    \item{p}{
        [qgev] - \cr
        a numeric vector of probabilities.
        [hillPlot] - \cr
        probability required when option \code{quantile} is
        chosen.
        }
    \item{q}{
        [pgev] - \cr
        a numeric vector of quantiles.
        }
    \item{x}{
        [dgev] - \cr
        a numeric vector of quantiles.
        }
    \item{xi, mu, beta}{
        [*gev] - \cr
        \code{xi} is the shape parameter, \code{mu} the location parameter,
        and \code{beta} is the scale parameter. The default values are 
        \code{xi=1}, \code{mu=0}, and \code{beta=1}. Note, if \code{xi=0} 
        the distribution is of type Gumbel.
        }   

}


\value{
    
    \code{d*} returns the density, \cr
    \code{p*} returns the probability, \cr
    \code{q*} returns the quantiles, and \cr
    \code{r*} generates random variates. \cr
    
    All values are numeric vectors.
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, 
    Springer.
    
}


\author{
  
    Alec Stephenson for R's \code{evd} and \code{evir} package, and \cr
    Diethelm Wuertz for this \R-port.
    
}

  
\examples{
## SOURCE("fExtremes.2A-GevModelling")

## *gev  - 
   # Create and plot 1000 GEV/Frechet distributed rdv:
   par(mfrow = c(3, 3))
   set.seed(4711)
   r = rgev(n = 1000, xi = 1)
   plot(r, type = "l", col = "steelblue", main = "Frechet Series")
   grid()
   ## Plot empirical density and compare with true density:
   ## Omit values greater than 500 from plot
   hist(r[r<10], n = 25, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = 1), col = "steelblue")
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   grid()
   q = seq(-5,5, by = 0.1)
   lines(q, pgev(q, xi = 1), col = "steelblue")
   # Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = 1), xi = 1) 
   
## *gev -
   # Create and plot 1000 Gumbel distributed rdv:
   r = rgev(n = 1000, xi = 0)
   plot(r, type = "l", col = "steelblue", main = "Gumbel Series")
   grid()
   # Plot empirical density and compare with true density:
   hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
      xlim = c(-5,5), ylim = c(0,1.1), main = "Density")
   x = seq(-5, 5, by = 0.01)
   lines(x, dgev(x, xi = 0), col = "steelblue")
   # Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
      cex = 0.5, ylab = "p", xlab="q", main="Probability")
   grid()
   q = seq(-5, 5, by = 0.1)
   lines(q, pgev(q, xi = 0), col = "steelblue")
   # Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = 0), xi = 0)   

## *gev -
   # Create and plot 1000 Weibull distributed rdv:
   r = rgev(n = 1000, xi = -1)
   plot(r, type = "l", col = "steelblue", main = "Weibull Series")
   grid()
   # Plot empirical density and compare with true density:
   hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
     xlim = c(-5,5), ylim=c(0,1.1), main="Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = -1), col = "steelblue")
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   grid()
   q = seq(-5, 5, by = 0.1)
   lines(q, pgev(q, xi = -1), col = "steelblue")
   # Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = -1), xi = -1)   
}


\keyword{models}

