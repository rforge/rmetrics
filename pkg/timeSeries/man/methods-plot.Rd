\name{plot-methods}


\title{Plot a Time Series}


\alias{plot}
\alias{plot,timeSeries-method}
\alias{lines,timeSeries-method}
\alias{points,timeSeries-method}
\alias{pretty.timeSeries}


\description{

    Plots 'timeSeries' objects and add lines and points. 
}


\usage{
\S4method{plot}{timeSeries}(x, y, FinCenter = NULL,
    plot.type = c("multiple", "single"), format = "auto",
    at = pretty(x), widths = 1, heights = 1, xy.labels,
    xy.lines, panel = lines, nc, yax.flip = FALSE,
    mar.multi = c(0, 5.1, 0, if (yax.flip) 5.1 else 2.1),
    oma.multi = c(6, 0, 5, 0), axes = TRUE, \ldots)

\S4method{lines}{timeSeries}(x, FinCenter = NULL, \dots)
\S4method{points}{timeSeries}(x, FinCenter = NULL, \dots)

\method{pretty}{timeSeries}(x, n=5, min.n=n\%/\%3, shrink.sml=0.75,
    high.u.bias=1.5, u5.bias=0.5+1.5*high.u.bias, eps.correct=0, \dots)
}


\details{

    The original plotting function \code{plot} was build along R's
    plotting function \code{plot.ts} with an additional argument
    to tailor the position marks at user defined position 
    specified by the argument \code{at}. We call this style or
    theme "ts".
    
    With Verison R 3.1 we have inroduced two new additionol
    plotting themes called "pretty" and "chick". They are becoming
    active when we set \code{at="pretty"} or \code{at="chic"}.
    
    Plot style or theme "pretty" is an extension of our original 
    plotting function.
    
    Plot style or theme "chic" an implementation along the contributed 
    packages \code{xts} and \code{PerformanceAnalytics} from the Chicago
    finance group members. "Chicago" gave the name to call the
    them \code{"chic"}.
    
    For both themes, "pretty" and "chic" additional arguments are passed 
    through the \code{\dots} arguments. These are:
    
    
    \tabular{lll}{
    \code{Argument:}  \tab Default: \tab Description: \cr
    \code{type}       \tab "l"  \tab types pf plot \cr
    \code{col}        \tab 1    \tab colors for lines and points \cr
    \code{pch}        \tab 20   \tab plot symbol \cr
    \code{cex}        \tab 1    \tab character and symbol scales \cr
    \code{lty}        \tab 1    \tab line types \cr
    \code{lwd}        \tab 2    \tab line widths \cr
    \code{cex.axes}   \tab 1    \tab scale of axes \cr
    \code{cex.lab}    \tab 1    \tab scale of labels \cr
    \code{cex.pch}    \tab 1    \tab scale of plot symbols \cr
    \code{}           \tab      \tab \cr
    \code{grid}       \tab TRUE \tab should grid lines plotted? \cr
    \code{frame.plot} \tab TRUE \tab should b box around the plot? \cr
    \code{axes}       \tab TRUE \tab should be axes drawn on the plot? \cr
    \code{ann}        \tab TRUE \tab should default annotations appear? }
    
    Concerning the plot elements, the length of these vectors has
    to be the same as the number of columns in the time series to
    be plotted. If their length is only one, then tey are repeated.
    
    There is an almost 70 pages vignette added to the package, with
    dozens of examples of tailored plots. Have a look in it.

}


\arguments{

    \item{x, y }{
        objects of class \code{timeSeries}.
        }
    \item{FinCenter}{
        a character with the the location of the  financial center
        named as \code{"continent/city"}.
        }
    \item{plot.type}{
        for multivariate time series, should the series by plotted
        separately (with a common time axis) or on a single plot?
        }
    \item{format}{POSIX label format, e.g. "\%Y-\%m-\%d" or "\%F" for
        ISO-8601 standard date format.
        }
    \item{at}{
        a \code{timeDate} object setting the plot label positions.
        If \code{at=pretty(x)}, the postitions are generated automatized
        calling the function \code{pretty}. Default option \code{at="auto"} 
        selects 6 equal spaced time label positions. For the new plot
        themes set at="pretty" or at="chic". In this case additional
        arguments can be passed through the \code{\dots} arguments,
        see details.
        }
    \item{widths, heights}{
        widths and heights for individual graphs, see \code{layout}.
        }
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
        \code{plot.type="multiple"}.  The default is \code{lines}.
        }
    \item{nc}{
        the number of columns to use when \code{type="multiple"}. Defaults
        to 1 for up to 4 series, otherwise to 2.
        }
    \item{yax.flip}{
        logical indicating if the y-axis (ticks and numbering) should
        flip from side 2 (left) to 4 (right) from series to series
        when \code{type="multiple"}.
        }
    \item{mar.multi, oma.multi}{
        the (default) \code{par} settings for \code{plot.type="multiple"}.
        }
    \item{axes}{
        logical indicating if x- and y- axes should be drawn.
        }
    \item{n}{
        an integer giving the desired number of intervals.
        }
    \item{min.n}{
        a nonnegative integer giving the minimal
        number of intervals.
        }
    \item{shrink.sml}{
        a positive numeric by a which a default
        scale is shrunk in the case when range(x) is
        very small.
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


\value{

   Displays  a plot or plot elements of an object of class 'timeSeries'.
    
}


\examples{
## Load Swiss Pension Fund Benchmark Data -
   LPP <- LPP2005REC[1:12, 1:4]
   colnames(LPP) <- abbreviate(colnames(LPP), 2)
   finCenter(LPP) <- "GMT"

## Example Plot 1 -
   plot(LPP[, 1], type = "o", col = "steelblue",
     main = "LPP", xlab = "2005", ylab = "Return")
   plot(LPP[, 1], at="auto", type = "o", col = "steelblue",
     main = "LPP", xlab = "2005", ylab = "Return")

## Example Plot 2 -
   plot(LPP[, 1:2], type = "o", col = "steelblue",
     main = "LPP", xlab = "2005", ylab = "Return")

## Example Plot 3 -
   plot(LPP[, 1], LPP[, 2], type = "p", col = "steelblue",
     main = "LPP", xlab = "Return 1", ylab = "Return 2")

## Example Plot 4a, The Wrong Way to do it! -
   LPP <- as.timeSeries(data(LPP2005REC))
   ZRH <- as.timeSeries(LPP[,"SPI"], zone = "Zurich", FinCenter = "Zurich")
   NYC <- as.timeSeries(LPP[,"LMI"], zone = "NewYork", FinCenter = "NewYork")
   finCenter(ZRH)
   finCenter(NYC)
   plot(ZRH, at="auto", type = "p", pch = 19, col = "blue")
   points(NYC, pch = 19, col = "red")

## Example Plot 4b, Convert NYC to Zurich Time -
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "Zurich"
   at <- unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at,
       xlab = paste(ZRH@FinCenter, "local Time"), main = ZRH@FinCenter)
   points(NYC, pch = 19, col = "red")

## Example 4c, Force Everything to GMT Using "FinCenter" Argument -
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "NewYork"
   at <- unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at,
       FinCenter = "GMT", xlab = "GMT", main = "ZRH - GMT")
   points(NYC, FinCenter = "GMT", pch = 19, col = "red")
}


\keyword{chron}

