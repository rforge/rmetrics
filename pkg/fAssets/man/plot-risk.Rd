\name{riskPlots}


\alias{seriesPlots}


\alias{assetsRiskReturnPlot}
\alias{assetsNIGShapeTrianglePlot}



\title{Assets Risk Plots}


\description{
  
    Displays risk plot from asseets.
}


\usage{
assetsRiskReturnPlot(x, col = "steelblue", percentage = FALSE, scale = 252, 
    labels = TRUE, add = TRUE, \dots)  
    
assetsNIGShapeTrianglePlot(x, labels, col = "steelblue", \dots)

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
    \item{percentage}{
        a logical flag. Are the returns given by log or percentual log
        returns?
        }
    \item{scale}{
        an integer value, the scale, i..e number of days, in a year. Used
        by daily data sets.
        }    
    \item{labels}{
        a logical flag, if \code{TRUE} then default labels will be used,
        otherwise the plots will be displayed without labels and the user
        can add his own labels.
        }          
    \item{add}{
        a logical flag, defining the color to fill the boxes.
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
   
## assetsRiskReturnPlot -
   # par(mfrow = c(2, 2))
   assetsRiskReturnPlot(LPP)
   
## assetsNIGShapeTrianglePlot -
   assetsNIGShapeTrianglePlot(LPP)
}

\keyword{models}
