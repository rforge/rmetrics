\name{QuantileQuantilePlots}

\alias{qqnormPlot}
\alias{qqnigPlot}
\alias{qqghtPlot}

\title{Quantile-Quantile Plots}


\description{
    
    Returns quantile-quantile plots for the normal,
    the normal inverse Gaussian, and the generalized 
    hyperbolic Student-t distribution.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{qqnormPlot} \tab Returns a tailored Normal quantile-quantile plot, \cr
    \code{qqnigPlot} \tab Returns a tailored NIG quantile-quantile plot, \cr
    \code{qqghtPlot} \tab Returns a tailored GHT quantile-quantile plot.}
    
}
    

\usage{     
qqnormPlot(x, labels = TRUE, col = "steelblue", 
    title = TRUE, grid = FALSE, rug = TRUE, scale = TRUE, \dots) 
qqnigPlot(x, labels = TRUE, col = "steelblue", 
    title = TRUE, grid = FALSE, rug = TRUE, scale = TRUE, \dots) 
qqghtPlot(x, labels = TRUE, col = "steelblue", 
    title = TRUE, grid = FALSE, rug = TRUE, scale = TRUE, \dots) 
}


\arguments{

    \item{col}{
        the color for the series. In the univariate case use just a 
        color name like the default, \code{col="steelblue"}, in the
        multivariate case we recommend to select the colors from a 
        color palette, e.g. \code{col=heat.colors(ncol(x))}. 
        }
    \item{grid}{
        a logical flag, should a grid be added to the plot? 
        By default \code{TRUE}. To plot a horizontal lines only
        use \code{grid="h"} and for vertical lines use \code{grid="h"},
        respectively.
        }
     \item{labels}{
        a logical flag, should the plot be returned with default labels
        and decorated in an automated way? By default \code{TRUE}.
        }   
    \item{rug}{
        a logical flag, by default TRUE. Should a rug representation 
        of the data added to the plot?
        }
    \item{scale}{
        a logical flag, by default TRUE. Should the time series be 
        scaled for the investigation?
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
## qqPlot - 
}


\keyword{programming}

