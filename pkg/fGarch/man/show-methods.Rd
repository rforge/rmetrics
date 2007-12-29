\name{show-methods}


\docType{methods}


\alias{show-methods}
\alias{show,ANY-method}
\alias{show,fGARCH-method}


\title{GARCH Show Methods}


\description{

    Show methods for GARCH Modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Print Function
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

