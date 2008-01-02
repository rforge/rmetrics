\name{plot-methods}


\docType{methods}


\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,fREG,missing-method}


\title{Regression Model Plot Methods}


\description{

    Plots results obtained from a fitted regression model. 

}


\usage{
\S4method{plot}{fREG,missing}(x, which = "ask", \dots)
}


\arguments{

    \item{x}{
        an object of class 'fREG'.
        } 
    \item{which}{
        a character string selectiong which plot should be displayed.
        By default \code{which="ask"} which allows to generate plots
        interactively.
        }
    \item{\dots}{
        additional arguments to mbe passed to the underlying plot
        functions.
        }
 
}


\section{Methods}{
    \describe{

    \item{x = "ANY", y = "ANY"}{
        Generic Function
        }
    \item{x = "fREG", y = "missing"}{
        Plot ... Function
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
   
## plot -
}


\keyword{models}

