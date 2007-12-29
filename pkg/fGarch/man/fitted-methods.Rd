\name{fitted-methods}


\docType{methods}


\alias{fitted-methods}
\alias{fitted,ANY-method}
\alias{fitted,fGARCH-method}


\title{Extract GARCH Model Fitted Values}


\description{

    Extracts fitted values from a fitted GARCH object. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Fitted
        }
        
    }
}


\note{

    \code{fitted} is a generic function which extracts fitted values 
    from objects returned by modeling functions.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 500)
  
## garchFit
   fit = garchFit(~garch(1, 1), data = x)
   
## formula - 
   fitted(fit)
}


\keyword{models}

