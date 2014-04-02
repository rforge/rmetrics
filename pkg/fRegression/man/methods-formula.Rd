\name{formula-methods}


\docType{methods}


\alias{formula-methods}
\alias{formula,ANY-method}
\alias{formula,fREG-method}


\title{Extract Regressiom Model formula}


\description{

    Extracts formula from a fitted regression model. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Formula
        }
        
    }
}


\note{

    \code{formula} is a generic function which extracts the formula
    expression from objects returned by modeling functions, here the 
    \code{regFit} and \code{gregFit} parameter estimation function. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## formula - 
   formula(fit)
}


\keyword{models}

