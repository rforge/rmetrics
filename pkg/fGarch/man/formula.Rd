\name{formula}


\alias{formula.fGARCH}


\title{Extract GARCH Model formula}


\description{

    Extracts formula from a fitted GARCH object. 
    
}


\usage{
\method{formula}{fGARCH}(x, \dots)
}


\arguments{

    \item{x}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{\dots}{
        additional arguments to be passed, not used.
        }   

}


\note{

    \code{formula} is a generic function which extracts the formula
    expression from objects returned by modeling functions. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 500)
  
## garchFit
   fit = garchFit(~garch(1, 1), data = x)
   
## formula - 
   formula(fit)
}


\keyword{models}

