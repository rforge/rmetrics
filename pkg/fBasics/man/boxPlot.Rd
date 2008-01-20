\name{BoxPlot}


\alias{boxPlot}
\alias{boxPercentilePlot}


\title{Time Series Box Plots}


\description{
    
    Returns a box or a box percentile plot.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{boxPlot} \tab Returns a side-by-side standard box plot, \cr
    \code{boxPercentilePlot} \tab Returns a side-by-side box-percentile plot.}
    
}
    

\usage{ 
boxPlot(x, col = "steelblue", title = TRUE, \dots)
boxPercentilePlot(x, col = "steelblue", title = TRUE, \dots) 
}


\arguments{
  
    \item{col}{
        the color for the series. In the univariate case use just a 
        color name like the default, \code{col="steelblue"}, in the
        multivariate case we recommend to select the colors from a 
        color palette, e.g. \code{col=heat.colors(ncol(x))}. 
        }
    \item{title}{
        a logical flag, by default TRUE. Should a default title added 
        to the plot?
        }
    \item{x}{
        an object of class \code{"timeSeries"} or any other object which
        can be transformed by the function \code{as.timeSeries} into an
        object of class \code{timeSeries}. The latter case, other then
        \code{timeSeries} objects, is more or less untested.
        }  
    \item{\dots}{
        optional arguments to be passed.
        }
    
}


\value{
    
    displays a time series plot.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## boxplot -
}


\keyword{programming}

