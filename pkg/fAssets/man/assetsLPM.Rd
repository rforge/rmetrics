\name{assetsLPM}

\alias{assetsLPM}


\title{Computation of Lower Partial Moments of Asset Sets}


\description{
  
    Computes assymmetric lower partial moments from a time 
    series of assets.   
}
    

\usage{
assetsLPM(x, tau, a, \dots)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }  
    \item{tau}{
        the target return.
        } 
    \item{a}{
        the value of the moment. 
        }    
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{
  
  
    returns a list with two entries named \code{mu} and \code{Sigma}.
    The first denotes the vector of lower partial moments, and the 
    second the co-LPM matrix. Note, that the output of this function 
    can be used as data input for the portfolio functions to compute 
    the LPM efficient frontier.
  
}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   
   # Lower Partial Moments:
   assetsLPM(LPP)
}


\keyword{models}

