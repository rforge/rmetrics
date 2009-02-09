\name{assetsQQNormPlot}


\alias{assetsQQNormPlot}


\title{Normal Quantile-Quantile Plots}


\description{
  
    Displays a normal quantile-quantile plot
}


\usage{
assetsQQNormPlot(x, col = "steelblue", skipZeros = FALSE, \dots)
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{col}{
        a character string, defining the color to fill the boxes.
        }
    \item{skipZeros}{
        a logical,  should zeros be skipped in the histogram plot of the
        return series?
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
   LPP = as.timeSeries(data(LPP2005REC))
    
## assetsQQNormPlot -
   # par(mfrow = c(2, 2))
   assetsQQNormPlot(LPP[, 1:3])
}

\keyword{models}

