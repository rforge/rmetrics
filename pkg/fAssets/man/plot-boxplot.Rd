\name{plot-boxplot}


\alias{boxPlot}

\alias{assetsBoxPlot}
\alias{assetsBoxPercentilePlot}  


\title{Displays a Box Plot of Assets}


\description{
  
    Displays standard box and box-percentile plots
    of assets.
   
}


\usage{
assetsBoxPlot(x, col = "bisque", \dots)
assetsBoxPercentilePlot(x, col = "bisque", \dots) 
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
## LPP -
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:6]
   head(LPP)
   
## assetsBoxPlot - 
   # Create a Boxplot: assetsBoxPlot - 
   assetsBoxPlot(LPP)
       
## assetsBoxPercentilePlot - 
   # Create a Box Percentile Plot: assetsBoxPercentilePlot - 
   assetsBoxPercentilePlot(LPP)  
   grid(NA, NULL, col="red")
}


\keyword{models}

