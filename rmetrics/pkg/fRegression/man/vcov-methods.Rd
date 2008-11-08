\name{vcov-methods}


\docType{methods}


\alias{vcov-methods}
\alias{vcov,ANY-method}
\alias{vcov,fREG-method}


\title{Extract Regression Model vcov}


\description{

    Extracts vcov from a fitted regression model. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fREG"}{
        Extractor function for vcov.
        }
        
    }
}


\note{

    \code{vcov} is a generic function which extracts fitted values 
    from objects returned by modeling functions, here the \code{regFit}
    and \code{gregFit} parameter estimation functions.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## vcov - 
   vcov(fit)
}


\keyword{models}

