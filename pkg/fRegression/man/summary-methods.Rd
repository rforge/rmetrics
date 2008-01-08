\name{summary-methods}


\docType{methods}


\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,fREG-method}


\title{Regression Summary Methods}


\description{

    Summary methods for regressing modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fREG"}{
        Summary method for objects of class 'fREG'.
        }
        
    }
}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## summary
   summary(fit)
}


\keyword{models}

