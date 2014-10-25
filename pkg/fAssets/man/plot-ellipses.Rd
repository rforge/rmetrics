\name{plot-ellipses}


\alias{covEllipsesPlot}


\title{Displays a Covariance Ellipses Plot}


\description{

    Displays a covariance ellipses plot.

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


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## LPP -
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:6]
   head(LPP)
   
## assetsMeanCov -
   # Compute Robust Covariance Matrix: assetsMeanCov - 
   Cov <- cov(LPP)
   robustCov <- assetsMeanCov(LPP, "MCD")$Sigma
   
## covEllipsesPlot -
   # Create Covariance Ellipse Plot: 
   covEllipsesPlot(list(Cov, robustCov))  
}


\keyword{models}

