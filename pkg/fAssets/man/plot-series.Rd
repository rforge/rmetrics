\name{plot-series}


\alias{seriesPlot}


\alias{assetsReturnPlot}
\alias{assetsCumulatedPlot}
\alias{assetsSeriesPlot}


\title{Displays Series Plots of Assets.}


\description{
  
    Displays series from sets of assets.
}


\usage{
assetsReturnPlot(x, col = "steelblue", \dots)
assetsCumulatedPlot(x, col = "steelblue", \dots)
assetsSeriesPlot(x, col = "steelblue", \dots)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeSeries}. 
        } 
    \item{col}{
        a character string, defining the color to fill the boxes.
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
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC
   head(LPP) 
    
## assetsReturnPlot -
   # Create Return Series Plot: 
   # par(mfrow = c(3, 2))
   assetsReturnPlot(LPP[, 1:3])
   
## assetsCumulatedPlot -
   # Create Cumulated Price/Index Plot: 
   assetsCumulatedPlot(LPP[, "LPP40"], col = "red")

## assetsSeriesPlot  
   # Crete Time Series Plot: 
   assetsSeriesPlot(LPP[, c("LMI", "ALT")], 
     col = c("orange", "brown"))
}


\keyword{models}
