\name{plot-methods}


\title{Plot a Time Series}


\alias{plot}
%\alias{plot,timeSeries,missing-method}
\alias{plot.timeSeries}
\alias{lines,timeSeries-method}
\alias{points,timeSeries-method}


\description{
    
    Plot 'timeSeries' objects.
    
}


\usage{
% \S4method{plot}{timeSeries, missing}(x, \dots) 
\method{plot}{timeSeries}(x, y = NULL, FinCenter = NULL, plot.type =
    c("multiple", "single"), format = "auto", at = "auto", widths = 1,
    heights = 1, xy.labels, xy.lines, panel = lines, nc, yax.flip =
    FALSE, mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1),
    oma.multi = c(6, 0, 5, 0), axes = TRUE, \ldots)
\S4method{lines}{timeSeries}(x, FinCenter = NULL, \dots)
\S4method{points}{timeSeries}(x, FinCenter = NULL, \dots)
}


\arguments{

  \item{x}{
    an object of class \code{timeSeries}.
  }
  \item{y}{
    an object of class \code{timeSeries}.
  }
  \item{FinCenter}{
    a character with the the location of the  financial center named 
    as "continent/city".
  }
  \item{plot.type}{
    for multivariate time series, should the series by plotted
    separately (with a common time axis) or on a single plot?
  }
  \item{format}{...}
  \item{at}{...}
  \item{widths}{...}
  \item{heights}{...}
  \item{xy.labels}{
    logical, indicating if \code{text()} labels should be used for an
    x-y plot, \_or\_ character, supplying a vector of labels to be
    used.  The default is to label for up to 150 points, and not
    for more.
  }
  \item{xy.lines}{
    logical, indicating if \code{lines} should be drawn for an x-y
    plot.  Defaults to the value of \code{xy.labels} if that is
    logical, otherwise to \code{TRUE}
  }
  \item{panel}{
    a \code{function(x, col, bg, pch, type, ...)} which gives the
    action to be carried out in each panel of the display for
    \code{plot.type="multiple"}.  The default is \code{lines}
  }
  \item{nc}{
    the number of columns to use when \code{type="multiple"}. Defaults
    to 1 for up to 4 series, otherwise to 2.
  }
  \item{yax.flip}{
    : logical indicating if the y-axis (ticks and numbering) should
    flip from side 2 (left) to 4 (right) from series to series
    when \code{type="multiple"}.
  }
  \item{mar.multi, oma.multi}{
    the (default) \code{par} settings for \code{plot.type="multiple"}.
  }
  \item{axes}{
    logical indicating if x- and y- axes should be drawn.
  }
  \item{\dots}{
    additional graphical arguments, see \code{plot}, \code{plot.default}
    and \code{par}
  }
}


\value{

    a plot or plot elements of an object of class \code{timeSeries}.
        
}


\examples{
## data -
   LPP = as.timeSeries(data(LPP2005REC))[1:12, 1:4]
   colnames(LPP) <- abbreviate(colnames(LPP), 2)
   finCenter(LPP) <- "GMT"

## plot -  
   plot(LPP[, 1], type = "o", col = "steelblue", 
     main = "LPP", xlab = "2005", ylab = "Return")
   plot(LPP[, 1:2], type = "o", col = "steelblue", 
     main = "LPP", xlab = "2005", ylab = "Return")
   plot(LPP[, 1], LPP[, 2], type = "o", col = "steelblue", 
     main = "LPP", xlab = "Return 1", ylab = "Return 2")
     
## plot -
   # Example 1 - The wrong way to do it!
   LPP = as.timeSeries(data(LPP2005REC))
   ZRH = as.timeSeries(LPP[,"SPI"], zone = "Zurich", FinCenter = "Zurich")
   NYC = as.timeSeries(LPP[,"LMI"], zone = "NewYork", FinCenter = "NewYork")
   finCenter(ZRH) 
   finCenter(NYC)
   plot(ZRH, type = "p", pch = 19, col = "blue")
   points(NYC, pch = 19, col = "red")
 
## plot - 
   # Example 2 - Convert NYC to Zurich Time
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "Zurich"
   at = unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at, 
       xlab = paste(ZRH@FinCenter, "local Time"), main = ZRH@FinCenter)
   points(NYC, pch = 19, col = "red")
  
## plot - 
   # Example 3 - Forced everything to GMT using "FinCenter" argument
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "NewYork"
   at = unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at, 
       FinCenter = "GMT", xlab = "GMT", main = "ZRH - GMT")
   points(NYC, FinCenter = "GMT", pch = 19, col = "red")
}


\keyword{chron}

