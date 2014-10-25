\name{TimeSeriesPlots}


\alias{seriesPlot}
\alias{returnPlot}
\alias{cumulatedPlot}
\alias{drawdownPlot}


\title{Financial Time Series Plots}


\description{
    
    Returns an index/price, a return, or a
    drawdown plot.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{seriesPlot} \tab Returns a tailored return series plot, \cr
    \code{cumulatedPlot} \tab Displays a cumulated series given the returns, \cr
    \code{returnPlot} \tab Displays returns given the cumulated series, \cr
    \code{drawdownPlot} \tab Displays drawdowns given the return series.}
    
}
    

\usage{ 
seriesPlot(x, labels = TRUE, type = "l", col = "steelblue", 
    title = TRUE, grid = TRUE, box = TRUE, rug = TRUE, \dots) 
cumulatedPlot(x, index = 100, labels = TRUE, type = "l", col = "steelblue", 
    title = TRUE, grid = TRUE, box = TRUE, rug = TRUE, \dots) 
returnPlot(x, labels = TRUE, type = "l", col = "steelblue", 
    title = TRUE, grid = TRUE, box = TRUE, rug = TRUE, \dots) 
drawdownPlot(x, labels = TRUE, type = "l", col = "steelblue", 
    title = TRUE, grid = TRUE, box = TRUE, rug = TRUE, \dots) 
}


\arguments{
  
    \item{box}{
        a logical flag, should a box be added to the plot? 
        By default \code{TRUE}.
        }
    \item{col}{
        the color for the series. In the univariate case use just a 
        color name like the default, \code{col="steelblue"}, in the
        multivariate case we recommend to select the colors from a 
        color palette, e.g. \code{col=heat.colors(ncol(x))}. 
        } 
    \item{grid}{
        a logical flag, should a grid be added to the plot? 
        By default \code{TRUE}.
        }
    \item{index}{
        a numeric value, by default 100. The function cumulates 
        column by colum the returns and multiplies the result with 
        the index value: \code{index*exp(colCumsums(x))}. 
        }
    \item{labels}{
        a logical flag, should the plot be returned with default labels
        and decorated in an automated way? By default \code{TRUE}.
        }   
    \item{rug}{
        a logical flag, by default TRUE. Should a rug representation 
        of the data added to the plot?
        }
    \item{title}{
        a logical flag, by default TRUE. Should a default title added 
        to the plot?
        }        
    \item{type}{
        what type of plot should be drawn? By default we use a line plot,
        \code{type="l"}. An alternative plot style which produces nice 
        figures is for example \code{type="h"}.
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


\details{

    The plot functions can be used to plot univariate and multivariate
    time series of class \code{timeSeries}.
    
    The graphical parameters \code{type} and \code{col} can be set by the
    values specified through the argument list. In the case of multivariate
    time series \code{col} can be specified by the values returned by
    a color palette.
    
    Automated titles including main title, x- and y-lables, grid lines,
    box style and rug represenatations cann be selected by setting these
    arguments to \code{TRUE} which is the default. If the title flag
    is unset, then the main title, x-, and y-labels are empty strings.
    This allows to set user defined labels with the function \code{title}
    after the plot is drawn.
    
    Beside \code{type}, \code{col}, \code{main}, \code{xlab} and 
    \code{ylab}, all other \code{par} arguments can be passed to the
    \code{plot} function.
    
    If the \code{labels} flag is unset to \code{FALSE}, then no decorations 
    will be added tothe  plot, and the plot can be fully decorated by the
    user.
    
} 
    


\value{
    
    displays a time series plot.
    
}


\examples{
## seriesPlot -
   tS <- as.timeSeries(data(LPP2005REC))
   seriesPlot(tS)
}


\keyword{programming}

