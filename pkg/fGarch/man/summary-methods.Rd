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
        Summary Function
        }
        
    }
}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
\dontrun{
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
}


\keyword{models}

