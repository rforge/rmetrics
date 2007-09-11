\name{garchExtractors}

\alias{garchExtractors}

\alias{residuals.fGARCH}
\alias{fitted.fGARCH}


\title{GARCH Extractor Functions}


\description{

    Extracts residuals and fitted values from a fitted GARCH object. 
}


\usage{
\method{residuals}{fGARCH}(object, \dots)
\method{fitted}{fGARCH}(object, \dots)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }   
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   x = garchSim(model = spec@model, n = 500)
   head(x) 

## garchFit - 
   # fit = garchFit(~garch(1, 1), data = x)
   # print(fit)
   ## Interactive Plot:
   ## plot(fit)
   ## Batch Plot:
   # plot(fit, which = 3)
   # summary(fit)
}


\keyword{models}

