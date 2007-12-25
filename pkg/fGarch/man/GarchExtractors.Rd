\name{garchExtractors}

\alias{garchExtractors}

\alias{residuals.fGARCH}
\alias{fitted.fGARCH}
\alias{volatility.fGARCH}


\title{GARCH Extractor Functions}


\description{

    Extracts residuals and fitted values from a fitted GARCH object. 
}


\usage{
\method{residuals}{fGARCH}(object, \dots)
\method{fitted}{fGARCH}(object, \dots)
\method{volatility}{fGARCH}(object, type = c("sigma", "h"), \dots)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{type}{
        [volatility] - \cr
        a character string, denoting which type of conditional values
        should be extracted. Either \code{"sigma"}, the default value,
        then conditional standard deviations are returned, or \code{"h"},
        then conditional variances will be returned.
        }
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 500)

## garchFit - 
   # fit = garchFit(~garch(1, 1), data = x)
   # volatility(fit)
}


\keyword{models}

