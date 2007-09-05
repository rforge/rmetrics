\name{PlotUtilities}

\alias{PlotUtilities}

\alias{interactivePlot}
\alias{gridVector}


\title{Explorative Data Analysis}


\description{

    A collection and description of functions for plotting
    with emphasis on interactive plots.
    \cr
      
    The plot utility functions are:
    
    \tabular{ll}{
    \code{interactivePlot} \tab a framework for interactive plot displays, \cr
    \code{gridVector} \tab creates from two vectors x and y all grid points. }
    
}


\usage{
interactivePlot(x, choices = paste("Plot", 1:9), 
    plotFUN = paste("plot.", 1:9, sep = ""), which = "all", \dots)
gridVector(x, y)
}


\arguments{

    \item{choices}{
        [interactivePlot] - \cr
        a vector of character strings for the
        choice menu. By Default \code{"Plot 1"} ... \code{"Plot 9"}
        allowing for 9 plots at maximum.
        }
    \item{plotFUN}{
        [interactivePlot] - \cr
        a vector of character strings naming the
        plot functions. By Default \code{"plot.1"} ... \code{"plot.9"}
        allowing for 9 plots at maximum.
        } 
    \item{which}{
        plot selection, which graph should be displayed? If \code{"which"}
        is a character string named "ask" the user is interactively asked 
        which to plot, if a logical vector of length \code{N}, those plots 
        which are set \code{TRUE} are displayed, if a character string
        named \code{"all"} all plots are displayed.
        }
    \item{x, y}{
        [gridVector] - \cr
        two numeric vector which span the two dimensional grid. 
        }
    \item{\dots}{
        additional arguments passed to the FUN or plot function.
        }
        
}


\author{

    Diethelm Wuertz for this R port.
    
}


\examples{
## Test Plot Function:
   testPlot = function(x, which = "all", ...) {   
     # Plot Function and Addons:
     plot.1 <<- function(x, ...) plot(x, ...)      
     plot.2 <<- function(x, ...) acf(x, ...)
     plot.3 <<- function(x, ...) hist(x, ...)      
     plot.4 <<- function(x, ...) qqnorm(x, ...)
     # Plot:
     interactivePlot(x,
       choices = c("Series Plot", "ACF", "Histogram", "QQ Plot"),
       plotFUN = c("plot.1", "plot.2", "plot.3", "plot.4"),
       which = which, ...)       
     # Return Value:
     invisible()
   }  
   # Plot:
   par(mfrow = c(2, 2), cex = 0.7)
   testPlot(rnorm(500))
      
   # Try:
   # par(mfrow = c(1,1))
   # testPlot(rnorm(500), which = "ask")
        
## Grid Vector - 
   gridVector(1:5, 1:5)
}


\keyword{hplot}

