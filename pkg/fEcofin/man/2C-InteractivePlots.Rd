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


\keyword{hplot}

