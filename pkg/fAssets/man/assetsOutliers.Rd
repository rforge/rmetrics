\name{assetsOutliers}


\alias{assetsOutliers}


\title{Detection of Outliers in Asset Sets}


\description{
  
    Detects outliers in asset sets.
    
}
    

\usage{
assetsOutliers(x, center, cov, \dots)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeSeries}. 
        }        
    \item{center}{ 
        a numeric vector, a (robust) estimate of the vector of means  
        of the multivariate time series \code{x}.
        }
    \item{cov}{
        a numeric matrix, a (robust) estimate of the covariance matrix 
        of the multivariate time series \code{x}.
        }
  
    \item{\dots}{
        optional arguments to be passed.
        }
        
}


\value{

    returns a list with 
    the estimate for the location named \code{center}, 
    the estimate for the covariance matrix named \code{cov},
    the estimate for the correlation matrix named \code{cor},  
    the quantile named \code{quantile}, and 
    the outliers named \code{outliers}. 
  
}


\author{

    Moritz Gschwandtner and Peter Filzmoser for the original R code
    from package "mvoutliers", \cr
    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   assetsOutliers(LPP, colMeans(LPP), cov(LPP))
}


\keyword{models}

