\name{show-methods}


\docType{methods}


\alias{show-methods}
\alias{show,ANY-method}
\alias{show,fGARCH-method}
\alias{show,fGARCHSPEC-method}


\title{GARCH Modelling Show Methods}


\description{

    Show methods for GARCH Modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fGARCH"}{
        Print function for objects of class \code{"fGARCH"}.
        } 
    \item{object = "fGARCHSPEC"}{
        Print function for objects of class \code{"fGARCH"}.
        }
        
    }
}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   print(spec)

## garchSim -
   x = garchSim(model = spec@model, n = 500) 

\dontrun{
## garchFit - 
   fit = garchFit(~ garch(1, 1), data = x)
   print(fit)
}
}


\keyword{models}

