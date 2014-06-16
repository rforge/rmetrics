\name{plot-methods}


\title{Plot a Time Series}


\alias{plot}
\alias{plot,timeSeries-method}
\alias{lines,timeSeries-method}
\alias{points,timeSeries-method}
\alias{pretty.timeSeries}


\description{

    Plotting method for objects inheriting from class 
    \code{"timeSeries"} objects.

}


\usage{
\S4method{plot}{timeSeries}(
   x, y, FinCenter = NULL, type = NULL, 
    plot.type = c("single", "multiple"),
    format = "auto", at = c("pretty", "chic"), minor.ticks="auto",
    col, pch, cex, lty,  lwd, 
    grid = TRUE, frame.plot = TRUE, panel = lines, 
    axes = TRUE, ann = TRUE, cex.axis=1, cex.lab=1, cex.pch=1,
    yax.flip = FALSE, 
    mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1), 
    oma.multi = c(7.75, 1.1, 6.1, 1.1),
    \dots)

\S4method{lines}{timeSeries}(x, FinCenter = NULL, \dots)
\S4method{points}{timeSeries}(x, FinCenter = NULL, \dots)

\method{pretty}{timeSeries}(x, n=5, min.n=n\%/\%3, shrink.sml=0.75,
    high.u.bias=1.5, u5.bias=0.5+1.5*high.u.bias,
    eps.correct=0, \dots)
}


\arguments{

    \item{x, y}{
        objects of class \code{timeSeries}.
        }
    \item{FinCenter}{
        a character with the the location of the  financial center
        named as \code{"continent/city"}.
        }
    \item{type}{
        what type of plot should be drawn? See \code{?plot} help.
        }
    \item{plot.type}{
        for multivariate time series, should the series be plotted
        separately (with a common time axis) or on a single plot graph?
        For univariate time series the argument \code{plot.type} will be
        forced to \code{"multiple"}, or short \code{"m"}.
        }       
    \item{format}{POSIX label format, e.g. "\%Y-\%m-\%d" or "\%F" for
        ISO-8601 standard date format. \code{"auto"} does automatic
        format selection.
        }
    \item{at}{
        a \code{timeDate} object setting the plot label positions. If 
        \code{at="pretty"}, the postitions are generated in an automatized 
        way calling the function \code{pretty}. If \code{at="chic"} then 
        postitions and format type are constructed according to the 
        \code{xts} tick axis algorithm.    
        }
    \item{minor.ticks}{
        a character, denoting the style of the minor ticks. By devault
        \code{"auto"}. Alternatively you can force the style bysetting
        the minor ticks to \code{"day"}, \code{"week"}, or \code{"month"}.
        }
    \item{col, pch, cex, lty,  lwd}{
        decorations for colors, plot characters, size, line type, and
        line width, see \code{?plot}.
        }    
    \item{grid}{
        a logical, should a grid added to the plot?
        }
    \item{frame.plot}{
        a logical, should a box frame added to the plot?
        }
    \item{panel}{   
        a \code{function(x, y, col, lty, \dots)} which gives the 
        action to be carried out in each panel of the display for 
        \code{plot.type = "multiple"}.
        }
    \item{axes}{
        logical indicating if x- and y- axes should be drawn.
        }
    \item{ann}{
        If set to \code{FALSE}, high-level plotting functions calling 
        \code{plot.default} do not annotate the plots they produce 
        with axis titles and overall titles. The default is to do 
        annotation.
        } 
    \item{cex.axis, cex.lab, cex.pch}{
        The magnification to be used for axis annotation relative to 
        the current setting of cex.
        }    
    \item{yax.flip}{
        logical indicating if the y-axis (ticks and numbering) should
        flip from side 2 (left) to 4 (right) from series to series
        when \code{type="multiple"}.
        }   
    \item{mar.multi, oma.multi}{
        the (default) \code{par} settings for \code{plot.type="multiple"}.
        }
    
    \item{n}{
        an integer giving the desired number of intervals.
        }
    \item{min.n}{
        a nonnegative integer giving the minimal number of intervals.
        }
    \item{shrink.sml}{
        a positive numeric by a which a default scale is shrunk in the 
        case when \code{range(x)} is very small.
        }
    \item{high.u.bias}{
        a non-negative numeric, typically > 1.
        Larger high.u.bias values favor larger units.
        }
    \item{u5.bias}{
        a non-negative numeric multiplier favoring
        factor 5 over 2.
        }
    \item{eps.correct}{
       an integer code, one of {0,1,2}. If
       non-0, a correction is made at the boundaries.
        }
    \item{\dots}{
       additional graphical arguments, see \code{plot}, \code{plot.default}
       and \code{par}.
      }
}


\details{
    
    If \code{y} is missing, the \code{plot} function creates a time 
    series plot, for multivariate series of one of two kinds depending 
    on \code{plot.type}.
    
    If \code{y} is present, both x and y must be univariate, and a 
    scatter plot \code{y ~ x} will be drawn, enhanced by using text 
    if \code{xy.labels} is \code{TRUE} or character, and lines if 
    \code{xy.lines} is \code{TRUE}.
    
    Note, the code of the function \code{plot} is build on top of the
    \code{plot.ts} function from the \code{stats} package.
}


\value{
    a plot or plot elements of an object of class \code{timeSeries}.
}


\examples{
## Load Swiss Pension Fund Benchmark Data -
   LPP <- LPP2005REC[1:12, 1:4]
   colnames(LPP) <- abbreviate(colnames(LPP), 2)
   finCenter(LPP) <- "GMT"

## Example Plot 1 -
   plot(LPP[, 1], type = "o", col = "steelblue", ann=FALSE)
   title(main = "LPP", ylab="Return")

## Example Plot 2 -
   plot(LPP[, 1:2], type = rep("o", 2), col = c("steelblue", "darkgreen"))
   title(main = "LPP")

## Example Plot 3 -
   plot(LPP[, 1], LPP[, 2], type = "p", col = "steelblue", ylab="")
   title(main = "LPP")

## Example Plot 4a, The Wrong Way to do it! -
   LPP <- as.timeSeries(data(LPP2005REC))
   ZRH <- as.timeSeries(LPP[,"SPI"], zone = "Zurich", FinCenter = "Zurich")
   NYC <- as.timeSeries(LPP[,"LMI"], zone = "NewYork", FinCenter = "NewYork")
   finCenter(ZRH)
   finCenter(NYC)
   plot(ZRH, type = "p", pch = 19, col = "blue")
   points(NYC, pch = 19, col = "red")

## Example Plot 4b, Convert NYC to Zurich Time -
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "Zurich"
   plot(ZRH, type = "p", pch = 19, col = "blue")
   points(NYC, pch = 19, col = "red")
   title(main = ZRH@FinCenter)
%
%## Example 4c, Force Everything to GMT Using "FinCenter" Argument -
%   finCenter(ZRH) <- "Zurich"
%   finCenter(NYC) <- "NewYork"
%   plot(ZRH, type = "p", pch = 19, col = "blue", FinCenter = "GMT")
%   points(NYC, FinCenter = "GMT", pch = 19, col = "red")
%   title(xlab = "GMT", main = "ZRH - GMT")
}


\keyword{chron}

