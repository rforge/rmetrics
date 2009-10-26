\name{tailDependence}


\alias{tailDependence}
\alias{tailDependenceCoeffs}


\title{Tail Dependence Coefficients}


\description{
  
    Estimates the tail dependence coefficients for a portfolio of assets.
    
}
    

\usage{
tailDependenceCoeffs(x, method = "nig", trace = FALSE, doplot = TRUE)
}



\arguments{

    \item{x}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. 
        }
    \item{method}{
        a character string naming the distribution of the assets returns.
        Currently the NIG distribution is the only supported one.
        }
    \item{trace}{
        a logical value, should the results of the estimation be traced?
        }
    \item{doplot}{
        a logical value, should the results of the estimation be plotted?
        }

}


\value{
    
    returns a list with two entries,
    the \code{upper} and \code{lower} tail dependence coefficients,
    and generates optionally a plot of copulae. 
    
}


\references{

Wuertz, D., Chalabi, Y., Ellis A., Theussl S. (2009);
    \emph{Advanced Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## tailDependenceCoeffs -
   # par(mfrow = c(4, 4))
   data = 100*LPP2005.RET[, 1:6]
   tailDependenceCoeffs(data)
}


\keyword{models}

