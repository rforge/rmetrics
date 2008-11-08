\name{show-methods}


\docType{methods}


\alias{show-methods}
\alias{show,ANY-method}
\alias{show,fREG-method}


\title{Regression Modelling Show Methods}


\description{

    Show methods for regression modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fREG"}{
        Print method for objects of class 'fREG'.
        } 
        
    }
}


\details{

    The show or print method returns the same information for all
    supported regression models through the \code{use} argument in
    the function \code{regFit}.
    
    These are the 'title', the 'formula', the 'family' and 
    the 'model parameters'.
    
}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## print -
   print(fit)
}


\keyword{models}

