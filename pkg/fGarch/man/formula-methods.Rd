\name{formula-methods}


\docType{methods}


\alias{formula-methods}
\alias{formula,ANY-method}
\alias{formula,fGARCH-method}


\title{Extract GARCH Model formula}


\description{

    Extracts formula from a formula GARCH object. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fGARCH"}{
        Extractor function for formula expression.
        }
        
    }
}


\note{

    \code{formula} is a generic function which extracts the formula
    expression from objects returned by modeling functions. 
    
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
   formula(fit)
}


\keyword{models}

