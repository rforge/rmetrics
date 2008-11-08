\name{coef-methods}


\docType{methods}


\alias{coef-methods}
\alias{coef,ANY-method}
\alias{coef,fREG-method}


\title{REG coefficients Methods}


\description{

    Extracts coefficients from a fitted regression model. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fREG"}{
        Extractor function for coefficients.
        }
        
    }
}


\note{

    \code{coef} is a generic function which extracts the coefficients
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
   
## coef - 
   coef(fit)
}


\keyword{models}

