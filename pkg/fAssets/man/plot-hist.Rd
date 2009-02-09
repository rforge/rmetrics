\name{histPlot}


\alias{histPlot}


\alias{assetsHistPlot}
\alias{assetsLogDensityPlot}


\title{Histogram Plots of Assets}


\description{
  
    Displays density of assets returns as a histogram and/or 
    as log density plot. 
    
}


\usage{
assetsHistPlot(x, col = "steelblue", skipZeros = FALSE, \dots)

assetsLogDensityPlot(x, estimator = c("hubers", "sample", "both"), 
    labels = TRUE, \dots)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{skipZeros}{
        a logical,  should zeros be skipped in the histogram plot of the
        return series ?
        }
    \item{col}{
        a character string, defining the color to fill the boxes.
        }
    \item{estimator}{
        a character string naming the type of estimator to fit the mean 
        and variance of the normal density. This may be either \code{"huber"}, 
        \code{"sample"}, or \code{"both"}.
        }        
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }
    \item{\dots}{
        optional arguments to be passed.
        }       
        
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
## LPP2005REC -
   x = as.timeSeries(data(LPP2005REC))
   
  
## assetsHistPlot -
   # par(mfrow = c(2, 2))
   assetsHistPlot(x[, 1:4])
   
## assetsLogDensityPlot -
   # par(mfrow = c(1, 1))
   assetsLogDensityPlot(x[, "ALT"], estimator = "both")

}

\keyword{models}
