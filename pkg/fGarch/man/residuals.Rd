\name{residuals}

\alias{residuals.fGARCH}


\title{Extract GARCH Model Residuals}


\description{

    Extracts residuals from a fitted GARCH object. 
}


\usage{
\method{residuals}{fGARCH}(object, \dots)
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

    \code{residuals} is a generic function which extracts residual values 
    from objects returned by modeling functions.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## residuals -
   # x = garchSim(n = 500)
   # fit = garchFit(~garch(1, 1), data = x)
   # residuals(fit)
}


\keyword{models}

