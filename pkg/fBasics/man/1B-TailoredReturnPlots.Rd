\name{TailoredReturnPlots}

\alias{TailoredReturnPlots}

\alias{seriesPlot}
\alias{histPlot}
\alias{densityPlot}
\alias{quantilePlot}


\title{Tailored Return Plots}


\description{
    
    A collection and description of functions which
    allow to create easily financial return plots.
    \cr

    The tailored plot functions are:
    
    \tabular{ll}{
    \code{seriesPlot} \tab Returns a tailored time series plot, \cr
    \code{histPlot} \tab Returns a tailored histogram plot, \cr
    \code{densityPlot} \tab Returns a tailored kernel density estimate plot, \cr
    \code{quantilePlot} \tab Returns a tailored quantile-quantile plot.}
    
}
    

\usage{ 
seriesPlot(x, col = "steelblue", main = x@units, \dots) 
histPlot(x, col = "steelblue", main = x@units, add.fit = TRUE, \dots) 
densityPlot(x, col = "steelblue", main = x@units, add.fit = TRUE, \dots)
quantilePlot(x, col = "steelblue", main = x@units, labels = TRUE, \dots) 
}


\arguments{
  
    \item{add.fit}{
        [*Plot] - \cr
        a logical, should a fit added to the Plot?
        }
    \item{col, main}{
        [*Plot] - \cr
        plot parameters, color and main title.
        } 
    \item{labels}{
        a logical, should labels be added to the plot?
        }
    \item{x}{
        an object of class \code{"timeSeries"}.
        }
    \item{\dots}{
        optional arguments to be passed.
        }
    
}


\value{
    
    Beside the plot, no other values are returned.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.1B-TailoredReturnPlots")
}


\keyword{programming}

