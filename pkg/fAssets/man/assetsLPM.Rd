\name{LowerPartialMoments}

\alias{LowerPartialMoments}

\alias{assetsLPM}


\title{Estimation of Lower Partial Moments of Asset Sets}


\description{
  
    A collection and description of functions 
    for the estimation of lower partial moments
    from a tinme series of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{assetsLPM} \tab Computes LPMs and co-LPMs of a set of assets. }
    
}
    

\usage{
assetsLPM(x, tau, a, \dots)
}

\arguments{
  

    \item{a}{
        the value of the moment. 
        }
    \item{tau}{
        the target return.
        }    
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{
  
  
    \code{assetsLPM}
    \cr
    returns a list with two entries named \code{mu} and \code{Sigma}.
    The first denotes the vector of lower partial moments, and the 
    second the co-LPM matrix. Note, that the output of this function 
    can be used as data input for the portfolio functions to compute 
    the LPM efficient frontier.
  
}


\seealso{

    \code{assetsMeanCov}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   
   # Lower Partial Moments:
   assetsLPM(LPP)
}


\keyword{models}
