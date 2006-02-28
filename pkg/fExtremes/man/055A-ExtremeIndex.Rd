\name{ExtremeIndexPlots}

\alias{ExtremeIndexPlots}

\alias{exindexPlot}
\alias{exindexesPlot}

\title{Extremal Index Estimation}


\description{
  
    A collection and description of functions to compute 
    the extremal index by three different kind of methods, 
    the blocks method, the reciprocal mean cluster size 
    method, and the runs method. 
    \cr
    
    The functiona are:
    
    \tabular{ll}{
    \code{exindexPlot} \tab Calculate and Plot Theta(1,2,3), \cr
    \code{exindexesPlot} \tab Calculate Theta(1,2) and Plot Theta(1). }
    
}


\usage{
exindexPlot(x, block = "month", start = 5, end = NA, plottype = c("thresh", 
    "K"), labels = TRUE, autoscale = TRUE, \dots)
    
exindexesPlot(x, block = 20, quantiles = seq(0.990, 0.999, 0.001), 
    doplot = TRUE, \dots)
}


\arguments{

    \item{autoscale}{
        [exindexPlot] - \cr
        whether or not plot should be automatically scaled; if not, 
        \code{xlim} and \code{ylim} graphical parameters may be entered.
        }
    \item{block}{
        the block size. A numeric value is interpreted as the number of 
        data values in each successive block.
        All the data is used, so the last block may not contain \code{block} 
        observations.
        If the \code{x} has a \code{times} attribute containing (in an 
        object of class \code{"POSIXct"}, or an object that can be
        converted to that class; see \code{\link{as.POSIXct}}) the
        times/dates of each observation, then \code{block} may instead
        take the character values \code{"month"}, \code{"quarter"},
        \code{"semester"} or \code{"year"}.
        Note, \code{exindexPlot} supports both numeric and character input,
        \code{exindexPlot} supports only numeric input.
        By default, monthly blocks or 20-day blocks are used which are 
        thought for daily data records.
        }
    \item{doplot}{
        [exindexesPlot] - \cr
        a logical, should the results be plotted?
        }
    \item{labels}{
        [exindexPlot] - \cr
        whether or not axes should be labelled.
        }
    \item{plottype}{
        [exindexPlot] - \cr
        whether plot is to be by increasing threshold (\code{thresh}) 
        or increasing K value (\code{K}).
        }
    \item{quantiles}{
        [exindexesPlot] - \cr
        a numeric vector of quantile values.
        }
    \item{start, end}{
        [exindexPlot] - \cr
        \code{start} is the lowest value of \code{K} at which to plot 
        a point, and \code{end} the highest value; \code{K} is the
        number of blocks in which a specified threshold is exceeded.
        }
    \item{x}{
        a numeric vector, note raw values are required, not block maxima.
        }
    \item{\dots}{
        additional arguments passed to the plot function.
        }
        
}


\value{
  
    \code{exindexPlot}
    \cr
    returns a data frame of results with the 
    following columns: \code{N}, \code{K}, \code{un}, \code{theta2},
    and \code{theta}. A plot with \code{K} on the lower x-axis and
    threshold Values on the upper x-axis versus the extremal index
    is displayed.
  
    \code{exindexesPlot}
    \cr
    returns a data.frame with four columns: 
    \code{thresholds}, \code{theta1}, \code{theta2}, and \code{theta3}.
    A plot with quantiles on the x-axis and versus the extremal indexes
    is displayed.
    
}


\author{

    Alexander McNeil, for the \code{exindexPlot} function, and \cr
    Diethelm Wuertz for the \code{exindexesPlot} function.
    
}


\references{

Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, 
    Springer. Chapter 8, 413--429.  
    
}


\seealso{
  
    \code{\link{hillPlot}},
    \code{\link{gevFit}}. 
  
}


\examples{
## SOURCE("fExtremes.55A-ExtremeIndex")

## Extremal Index for the right and left tails 
## of the BMW log returns:
   xmpExtremes("\nStart: Plot the Extremal Index >")
   data(bmw)
   par(mfrow = c(2, 2), cex = 0.7)
   exindexPlot( bmw, block = "quarter")
   exindexPlot(-bmw, block = "quarter")   
   
## Extremal Index for the right and left tails 
## of the BMW log returns:
   xmpExtremes("\nNext: Investigate Tail Depth Dependence >")
   data(bmw)
   exindexesPlot( bmw, block = 65)
   exindexesPlot(-bmw, block = 65)   
}


\keyword{hplot}

