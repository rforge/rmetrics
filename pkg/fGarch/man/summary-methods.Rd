\name{summary-methods}


\docType{methods}


\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,fGARCH-method}


\title{GARCH Summary Methods}


\description{

    Summary methods for GARCH Modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Summary function for objects of class \code{"fGARCH"}.
        }
        
    }
}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()

## garchSim -
   x = garchSim(model = spec@model, n = 500) 

## garchFit - 
   fit = garchFit(~ garch(1, 1), data = x)
   summary(fit)
}


\keyword{models}

