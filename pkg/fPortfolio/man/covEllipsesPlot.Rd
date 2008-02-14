\name{covEllipsesPlot}


\alias{covEllipsesPlot}


\title{Covariance Ellipses Plot}


\description{

    Plots covariance ellipses.

}


\usage{
covEllipsesPlot(x = list(), \dots)
}


\arguments{

    \item{x}{
        a list of at least two covariance matrices.
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }

}


\details{

    This plot visualizes the difference between two or more covariance matrices.
    It is meant to compare different methods of covariance estimation.

}



%\references{}


\examples{
## data -
   LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
   head(LPP)
   
## cov - 
   Cov = cov(LPP)
   robustCov = covMcdEstimator(LPP)$Sigma
   
## covEllipsesPlot -
   covEllipsesPlot(list(Cov, robustCov))  
}


\keyword{models}

