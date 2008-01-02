\name{residuals-methods}


\docType{methods}


\alias{residuals-methods}
\alias{residuals,ANY-method}
\alias{residuals,fREG-method}


\title{Extract Regression Model Residuals}


\description{

    Extracts residuals from a fitted regression object. 
}


\usage{
\S4method{residuals}{fREG}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fREG} as returned from the function
        \code{regFit()} or \code{gregFit()}.
        }    

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fREG"}{
        Residuals
        }
        
    }
}


\note{

    \code{residuals} is a generic function which extracts residual values 
    from objects returned by modeling functions.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
\dontrun{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## residuals - 
   residuals(fit)
}
}


\keyword{models}

