\name{fitted}


\alias{fitted.fGARCH}



\title{Extract GARCH Model Fitted Values}


\description{

    Extracts fitted values from a fitted GARCH object. 
    
}


\usage{
\method{fitted}{fGARCH}(object, \dots)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\note{

    \code{fitted} is a generic function which extracts fitted values 
    from objects returned by modeling functions. \code{fitted.values} 
    is an alias for it. 

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
   # x = garchSim(n = 500)
   # fit = garchFit(~garch(1, 1), data = x)
   # fitted(fit, type = "h"))
}


\keyword{models}

