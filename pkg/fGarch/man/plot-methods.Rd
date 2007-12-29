\name{plot-methods}


\docType{methods}


\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,fGARCH,missing-method}


\title{GARCH Plot Methods}


\description{

    Plot methods for GARCH Modelling. 

}


\usage{
\S4method{plot}{fGARCH,missing}(x, which = "ask", \dots)
}


\arguments{

    \item{x}{
        description
        } 
    \item{which}{
        description
        }
    \item{\dots}{
        description
        }
 
}


\section{Methods}{
    \describe{

    \item{x = "ANY", y = "ANY"}{
        Generic Function
        }
    \item{x = "fGARCH", y = "missing"}{
        Plot ... Function
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

