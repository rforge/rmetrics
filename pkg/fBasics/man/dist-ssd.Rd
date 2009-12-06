\name{ssd}


\alias{ssd}

\alias{dssd}
\alias{pssd}
\alias{qssd}
\alias{rssd}


\title{Spline Smoothed Distribution}


\description{
    
    Density, distribution function, quantile function 
    and random generation from smoothing spline estimates.
    
}


\usage{
dssd(x, param, log = FALSE)
pssd(q, param)
qssd(p, param)
rssd(n, param)
}


\arguments{
 
    \item{param}{
        an object as returned by the function \code{ssdFit}..
        }
    \item{log}{
        a logical flag by default \code{FALSE}. 
        Should labels and a main title drawn to the plot?
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

    All values for the \code{*ssd} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


\author{

    Diethelm Wuertz, Chong Gu for the underlying \code{gss} package.
    
}


\references{

Gu, C. (2002), 
    \emph{Smoothing Spline ANOVA Models}, 
    New York Springer--Verlag.

Gu, C. and Wang, J. (2003), 
    \emph{Penalized likelihood density estimation: 
    Direct cross-validation and scalable approximation},
    Statistica Sinica, 13, 811--826. 
    
}


\examples{   
## ssdFit -
   set.seed(1953)
   r = rnorm(500)
   hist(r, breaks = "FD", probability = TRUE,
     col = "steelblue", border = "white")
 
## ssdFit - 
   param = ssdFit(r)
   
## dssd -  
   u = seq(min(r), max(r), len = 301)
   v = dssd(u, param)
   lines(u, v, col = "orange", lwd = 2)
}


\keyword{distribution}

