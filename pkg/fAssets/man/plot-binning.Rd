\name{binningPlot}


\alias{binningPlot}


\alias{assetsHistPairsPlot} 



\title{Bivariate Histogram Plots of Assets}


\description{
  
    Displays bivariate histogram plots of assets returns. 
    
}


\usage{
assetsHistPairsPlot(x, bins = 30, method = c("square", "hex"), \dots) 
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{as.matrix()} into a matrix object, e.g. like an 
        object of class \code{timeSeries}, \code{data.frame}, or \code{mts}. 
        } 
    \item{bins}{
        an integer value, the number of bins used for the biariate 
        histogram.
        }
     \item{method}{
        a character string denoting whic h type of binning should be
        used, either \code{"squared"} or \code{"hexagonal"}.
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
     
## assetsHistPairsPlot -
   assetsHistPairsPlot(LPP[, c("LMI", "ALT")])
   assetsHistPairsPlot(LPP[, c("LMI", "ALT")], method = "hex")
}

\keyword{models}
