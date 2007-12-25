\name{volatility}


\alias{volatility.fGARCH}



\title{Extract GARCH Model Volatility}


\description{

    Extracts volatility from a fitted GARCH object. 
    
}


\usage{
\method{volatility}{fGARCH}(object, \dots)
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

    \code{volatility} is a generic function which extracts volatility values 
    from objects returned by modeling functions. 
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
   # x = garchSim(n = 500)
   # fit = garchFit(~garch(1, 1), data = x)
   # volatility(fit, type = "h"))
}


\keyword{models}

