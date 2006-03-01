\name{PlotFunctions}

\alias{PlotFunctions}

\alias{tsPlot}
\alias{histPlot}
\alias{densityPlot}
\alias{circlesPlot}
\alias{perspPlot}
\alias{contourPlot}

\alias{characterTable}
\alias{plotcharacterTable}
\alias{colorTable}
\alias{greyPal}

\alias{splusLikePlot}


\title{Basic Plot Functions and Utilities}


\description{ 

    A collection and description of several plot functions 
    and utilities which may be useful for the explorative 
    data analysis of financial and economic market data using
    S4 time series objects from Rmetrics. Included are also 
    utility functions displaying tables for characters, plot 
    symbols, and colors.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{tsPlot} \tab Time Series Plot, \cr
    \code{histPlot} \tab Histogram Plot of (Series) Data, \cr
    \code{densityPlot} \tab Kernel Density Estimate Plot, \cr
    \code{circlesPlot} \tab Scatterplot of Circles Indexing a 3rd Variable, \cr
    \code{perspPlot} \tab Perspective Plot in 2 Dimensions, \cr
    \code{perspPlot} \tab Contour Plot in 2 Dimensions, \cr
    \code{characterTable} \tab Table of Numerical Equivalents to Latin Characters, \cr
    \code{plotcharacterTable} \tab Table of plot characters, plot symbols, \cr
    \code{colorTable} \tab Table of Color Codes and Plot Colors itself, \cr
    \code{greyPal} \tab Creates a grey palette like rainbow does for colors, \cr
    \code{splusLikePlot} \tab Scales plotting and symbols.}
    
}


\usage{ 
tsPlot(x, \dots) 
histPlot(x, col = "steelblue4", border = "white", main = x@units, 
    add.fit = TRUE, \dots)
densityPlot(x, col = "steelblue4", main = x@units, add.fit = TRUE, \dots)

circlesPlot(x, y, size = 1, \dots)
perspPlot(x, y, z, theta = -40, phi = 30, col = "steelblue4", ps = 9, \dots)
contourPlot(x, y, z, \dots)

characterTable(font = 1, cex = 0.7)
plotcharacterTable(font = par('font'), cex = 0.7)
colorTable(cex = 0.7)
greyPal(n = 64, start = 255-n, end = 255)
}


\arguments{
  
    \item{add.fit}{
        [histPlot][densityPlot] - \cr
        a logical flag, if set to \code{TRUE} which is the default
        value than a normal fit will be added to the plot, otherwise
        not.
        }
    \item{col, border}{
        two character strings, defining the colors used to fill the
        bars and to plot the borders of the bars for the \code{histPlot},
        or color and linetype as used by the function \code{plot}.
        }
    \item{font, cex}{
        an integer value, the number of the \code{font}, by default font 
        number 1, the standard font for the \code{characterTable}
        or the current plot character font for the 
        \code{plotcharacterTable}. The character size is determined by
        the numeric value \code{cex}, the default size is 0.7.
        }  
    \item{main}{
        character string of main title(s).
        } 
    \item{n, start, end}{
        [greyPal] - cr
        \code{n} gives the number of greys to be constructed,
        \code{start} and \code{end} span the range of the color palette.
        By default 64 grey tones equidistant chosen from the color range 
        (191, 191, 191) to (255, 255, 255).
        } 
    \item{size}{
        a numeric vector like \code{z}, the third variable in the 
        function \code{circlesPlot}. The argument gives the size of the 
        circles, by default all values are set to 1.
        }
    \item{theta, phi, ps}{
        plot parameters for the function \code{perspPlot} as used by the
        function \code{persp}.
        }
    \item{x, y}{
        [tsPlot][histPlot][densityPlot] - \cr
        an object of class \code{timeSeries}.
        \cr
        [circlesPlot][perspPlot] - \cr
        numeric vectors.
        In the case of the \code{thermometerPlot} \code{x} holds the
        current values of the n-bars.
        }
    \item{z}{
        a matrix containing the values to be plotted by the function
        \code{perspPlot}.
        }
    \item{\dots}{
        arguments to be passed to the underlying plot function.
        }
    
}


\value{
    
    \code{tsPlot}
    \cr
    plots a time series. Just a synonyme call to the function
    \code{ts.plot} changing plot type to \code{l}ines and plot color 
    to \code{steelblue3}.
    \cr
    
    \code{histPlot}
    \cr
    plots a histogram. This is a synonyme function call for R's 
    histogram plot, calling the function \code{hist}. Returns an 
    object of class \code{"histogram"}, see \code{hist}.
    \cr
    
    \code{densityPlot}
    \cr
    returns an object of class \code{"density"}, see \code{density}.
    \cr
    
    \code{circlesPlot}
    \cr
    a simple pseudo three dimensional scatterplot of circels whose
    sizes index a thrid variable.
    \cr
    
    \code{perspPlot}\cr
    \code{contourPlot}
    \cr
    draws perspective or contour plots of surfaces over the x-y plane.
    \cr
    
    \code{characterTable}
    \cr
    displays a table with the characters of the requested font.
    The character on line "xy" and column "z" of the table has 
    code \code{"\xyz"}, e.g \code{cat("\\126")} prints: V for font 
    number 1. These codes can be used as any other characters.
    \cr 
    
    \code{plotcharacterTable}
    \cr
    displays a table with the plot characters numbered from 0 to 255.
    \cr
    
    \code{colorTable}
    \cr
    displays a table with the plot colors with the associated color number.
    
}


\details{

    \bold{Series Plots:}
    \cr\cr
    \code{tsPlot} plots time series on a common plot. Unlike \code{plot.ts} 
    the series can have a different time bases, but they should have the 
    same frequency. \code{tsPlot} is a synonyme function call for R's 
    \code{ts.plot} from the the \code{ts} package.
    \cr
    
    \bold{Histogram and Density Plots:}
    \cr\cr
    \code{histPlot} and \code{densityPlot} show (return) distributions in form 
    of a  histogram and density plots. The first is a synonyme function call 
    for R's histogram plot, calling the function \code{hist}. The outlook 
    of the plot is more SPlus like. The second creates a density Plot 
    with underlying kernel density estimation. It is a function call to  
    R's \code{density} function.
    \cr
    
    \bold{Three Dimensional Plots:}
    \cr\cr
    \code{circlesPlot} and \code{perspPlot}, see also \code{contour},
    are functions to plot 3 dimensional data sets. The first is
    simple scatterplot with points replaced with variable circles, 
    whose size indexes a third variable. The second aloows to create
    a perspective 3 dimensional plot. It is a function call to R's
    \code{persp} plot, but the parameters are setted to produce a
    more SPlis like outlook of the plot. \code{contour} is the call
    to R's contour plot from the \code{base} package.
    \cr

    \bold{Plot Utilities:}
    \cr\cr
    \code{characterTable}, \code{plotcharacterTable} and \code{colorTable}
    are three helpful utilities for using characters and colors in plots.
    The first function displays a table of numerical equivalents to Latin 
    characters, the second displays a table of plot characters, i.e. 
    the symbols used in plots, and the third displays a table with the
    codes of the default plot colors.
    
}


\author{
 
    Gordon Smyth for the circlesPlot, \cr
    Pierre Joyet for the characterTable, and \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.012B-PlotFunctions")

\dontrun{
## tsPlot -
   xmpBasics("\nStart: European Stock Markets > ")
   # Show multiple plot:
   par(mfrow = c(1, 1), cex = 0.7)
   data(DowJones30)
   DowJones.ts = as.timeSeries(DowJones30)[, c("CAT", "GE", "IBM", "JPM", )]
   tsPlot(DowJones.ts)
   title(main = "CAT - GE - IBM - JPM")
   
## histPlot -
   xmpBasics("\nNext: Histogram Plot of Normal Random Numbers > ")
   DowJones.ret = returnSeries(DowJones.ts)
   par(mfrow = c(2, 2), cex = 0.7)
   histPlot(x = DowJones.ret)
   
## densityPlot -
   xmpBasics("\nNext: Density Plot of Normal Random Numbers > ")
   densityPlot(x = DowJones.ret)
   
## circlesPlot -
   xmpBasics("\nNext: 3D Circles Plot of Normal Random Numbers > ")
   par(mfrow = c(1, 1), cex = 0.7)
   circlesPlot(x = rnorm(50), y = rnorm(50), size = abs(rnorm(50)),
     main = "Circles Plot")

## perspPlot -
   xmpBasics("\nNext: Perspective Plot > ")
   par(mfrow = c(1, 1))
   x = y = seq(-10, 10, length = 51)
   f = function(x, y) { r = sqrt(x^2+y^2); 10 * sin(r)/r }
   z = outer(x, y, f)   
   perspPlot(x, y, z)
   title(main = "Perspective Plot", line = -3)
   
## characterTable - 
   xmpBasics("\nNext: Print the Copyright Sign > ")
   cat("\251 \n")

## characterTable - 
   xmpBasics("\nNext: Display Character Table for Symbol Font > ")
   characterTable(5)
   
## colorTable - 
   xmpBasics("\nNext: Display Table of Plot Colors > ")
   colorTable()
   
## plotcharacter Table - 
   xmpBasics("\nNext: Display Table of Plot Characters > ")
   plotcharacterTable()
}
}


\keyword{hplot}

