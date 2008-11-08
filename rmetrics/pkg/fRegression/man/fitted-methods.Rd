\name{fitted-methods}


\docType{methods}


\alias{fitted-methods}
\alias{fitted,ANY-method}
\alias{fitted,fREG-method}


\title{Extract Regression Model Fitted Values}


\description{

    Extracts fitted values from a fitted regression model. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fREG"}{
        Extractor function for fitted values.
        }
        
    }
}


\note{

    \code{fitted} is a generic function which extracts fitted values 
    from objects returned by modeling functions, here the \code{regFit}
    and \code{gregFit} parameter estimation functions.
    
    The class of the fitted values is the same as the class of the 
    data input to the function \code{regFit} or \code{gregFit}. In
    contrast the slot \code{fitted} returns a numeric vector.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x.df = regSim(model = "LM3", n = 50)
  
## regFit -
   # Use data.frame input:
   fit = regFit(Y ~ X1 + X2 + X3, data = x.df, use = "lm")
   
## fitted - 
   val = slot(fit, "fitted")
   head(val)
   class(val)
   val = fitted(fit)
   head(val)
   class(val)
   
## regFit -
   # Convert to dummy timeSeries Object:
   x.tS = as.timeSeries(x.df)
   fit = regFit(Y ~ X1 + X2 + X3, data = x.tS, use = "lm")
   
## fitted - 
   val = slot(fit, "fitted")
   head(val)
   class(val)
   val = fitted(fit)
   head(val)
   class(val)
}


\keyword{models}

