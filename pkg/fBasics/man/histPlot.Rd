\name{HistogramPlot}


\alias{histPlot}
\alias{densityPlot}
\alias{logDensityPlot}


\title{Return Series Plots}


\description{
    
    Returns a histogram, a density, or a logarithmic 
    density plot.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab Returns a tailored kernel density estimate plot, \cr
    \code{logDensityPlot} \tab Returns a tailored log kernel density estimate plot.}
    
}
    

\usage{        
histPlot(x, labels = TRUE, col = "steelblue", fit = TRUE, 
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots) 
densityPlot(x, labels = TRUE, col = "steelblue", fit = TRUE, hist = TRUE, 
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots)    
logDensityPlot(x, labels = TRUE, col = "steelblue", robust = TRUE,  
    title = TRUE, grid = TRUE, rug = TRUE, skip = FALSE, \dots)    
}


\arguments{
  
    \item{col}{
        the color for the series. In the univariate case use just a 
        color name like the default, \code{col="steelblue"}, in the
        multivariate case we recommend to select the colors from a 
        color palette, e.g. \code{col=heat.colors(ncol(x))}. 
        }
    \item{fit}{
        a logical flag, should a fit added to the Plot?
        }
    \item{grid}{
        a logical flag, should a grid be added to the plot? 
        By default \code{TRUE}. To plot a horizontal lines only
        use \code{grid="h"} and for vertical lines use \code{grid="h"},
        respectively.
        }
    \item{hist}{
        a logical flag, by default TRUE. Should a histogram to be 
        underlaid to the plot?
        }
    \item{labels}{
        a logical flag, should the plot be returned with default labels
        and decorated in an automated way? By default \code{TRUE}.
        }   
    \item{rug}{
        a logical flag, by default TRUE. Should a rug representation 
        of the data added to the plot?
        }
    \item{skip}{
        a logical flag, should zeros be skipped in the return Series?
        }
    \item{robust}{
        a logical flag, by default TRUE. Should a robust fit added to
        the plot?
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
## histPlot
}


\keyword{programming}

