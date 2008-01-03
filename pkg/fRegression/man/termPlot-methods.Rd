\name{termPlot-methods}


\docType{methods}


\alias{termPlot-methods}
\alias{termPlot,ANY-method}
\alias{termPlot,fREG-method}


\title{Regression Model Plot Methods}


\description{

    Plots results obtained from a fitted regression model. 

}


\usage{
\S4method{termPlot}{fREG}(model, \dots)
}


\arguments{

    \item{model}{
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
        Term plot function. 
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

