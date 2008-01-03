\name{terms-methods}


\docType{methods}


\alias{terms-methods}
\alias{terms,ANY-method}
\alias{terms,fREG-method}


\title{Regression Model Plot Methods}


\description{

    Plots results obtained from a fitted regression model. 

}


\usage{
\S4method{terms}{fREG}(x, \dots)
}


\arguments{

    \item{x}{
        an object of class 'fREG'.
        } 
    \item{\dots}{
        additional arguments to be passed to the underlying 
        functions.
        }
 
}


\section{Methods}{
    \describe{

    \item{x = "ANY"}{
        Generic function.
        }
    \item{x = "fREG"}{
        Terms extractor function. 
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
}


\keyword{models}

