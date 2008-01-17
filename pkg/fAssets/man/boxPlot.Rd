\name{boxPlot}


\alias{boxPlot}

\alias{assetsBoxPlot}
\alias{assetsBoxPercentilePlot}  


\title{Assets Box Plots}


\description{
  
    A collection and description of functions which 
    display several different kind of views on
    multivariate data sets of assets.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    
    % BOX PLOTS:
    \code{assetsBoxPlot} \tab Displays standard box plots, \cr
    \code{assetsBoxPercentilePlot} \tab Displays side-by-side box-percentile plots.}
    
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


\seealso{

    \code{MultivariateDistribution}.

}


\author{

    Diethelm Wuertz for the Rmetrics port.
    
}


\examples{
## LPP2005REC -
   x = as.timeSeries(data(LPP2005REC))
   head(x)
   
## assetsBoxPlot - 
   assetsBoxPlot(x)
       
## assetsBoxPercentilePlot - 
   assetsBoxPercentilePlot(x)   
}


\keyword{models}

