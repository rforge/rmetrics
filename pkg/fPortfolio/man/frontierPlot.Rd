\name{frontierPlot}


\alias{frontierPlot}

\alias{minvariancePoints}
\alias{cmlPoints}
\alias{cmlLines}
\alias{tangencyPoints}
\alias{tangencyLines}
\alias{equalWeightsPoints}
\alias{singleAssetPoints}
\alias{twoAssetsLines}
\alias{sharpeRatioLines}

\alias{monteCarloPoints}


\title{Efficient Frontier Plot}


\description{

    Plots the efficient frontier of an 
    optimized portfolio and allows to add
    points and lines from specif portfolios
    
    The functions are:
   
    \code{frontierPlot} \tab Plots efficient frontier, \cr
    \code{minvariancePoints} \tab Adds minimum variance point, \cr
    \code{cmlPoints} \tab Adds market portfolio, \cr
    \code{cmlLines} \tab  Adds capital market Line, \cr
    \code{tangencyPoints} \tab Adds tangency portfolio point, \cr
    \code{tangencyLines} \tab Adds tangency line, \cr
    \code{equalWeightsPoints} \tab Adds point of equal weights portfolio, \cr
    \code{singleAssetPoints} \tab Adds points of single asset portfolios, \cr
    \code{twoAssetsLines} \tab Adds EF for all combinations of two assets, \cr
    \code{sharpeRatioLines} \tab Adds Sharpe ratio line, \cr
    \code{monteCarloPoints} \tab Adds randomly produced feasible portfolios. }

}


\usage{
frontierPlot(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, \dots)

minvariancePoints(object, \dots)
cmlPoints(object, \dots)
cmlLines(object, \dots)
tangencyPoints(object, \dots)
tangencyLines(object, \dots)
equalWeightsPoints(object, \dots)
singleAssetPoints(object, \dots)
twoAssetsLines(object, \dots)
sharpeRatioLines(object, \dots)

monteCarloPoints(object, mcSteps = 5000, \dots)
}


\arguments{

    \item{add}{
        a logical value, determining whether the frontier should be added to 
        an existing plot, by default FALSE.
        }
    \item{col}{
        a character string vector, setting the color. For \code{frontierPlot} 
        it is a two dimensional a vector; first entry is the upper part of the
        frontier,\cr second entry the lower, by default "black" and "grey".\cr
        For the other functions the argument defines the color representation,
        by default sets the default color is the rainbow palette.
        }
    \item{frontier}{
        a character string, determining which part of the frontier should be
        extracted. \code{"both"} stands for the full hyperbola, \code{"lower"}
        for all points below the minimum variance return and \code{"upper"}
        for the actual efficient frontier, by default "both".
        }
    \item{mcSteps}{
        an integer value, the number of Monte Carlo steps.
        }
    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, containing slots call, data,
        specification, constraints, portfolio, title, description.
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }

}


\details{

    \bold{Frontier Plot:}
    \cr\cr
    The frontier plot provides plotting three parts of the efficient frontier.
    The part superior to the minimum variance portfolio is plotted with argument
    frontier = "upper". The part below is plotted with "lower". Both parts are
    plotted with "both", which is default. The ranges of plots are determined 
    by maximum of the ranges of the assets or the minimum and maximum of the
    efficient frontier. Adding an efficient frontier to an existing plot is
    possible with the argument add. The argument col determines the color of 
    the upper and lower part of the efficient frontier.

}



%\references{}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.

}


\examples{
## data -
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data

## portfolioFrontier -
   # Compute Long Only Efficient Frontier
   Frontier = portfolioFrontier(Data)
   Frontier
   
## frontierPlot -  
   # Plot Upper Part of Frontier:
   frontierPlot(Frontier, pch = 19, xlim = c(0, 0.25), ylim = c(0, 0.035))
   grid()
   abline(h = 0, col = "grey")
   abline(v = 0, col = "grey")
   
## Plot Addons:
   minvariancePoints(Frontier, pch = 19, col = "red")
   tangencyPoints(Frontier, pch = 19, col = "blue")
   tangencyLines(Frontier, col = "blue")
   equalWeightsPoints(Frontier, pch = 15, col = "grey")
   singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = topo.colors(6))
   twoAssetsLines(Frontier, lty = 3, col = "grey")
   sharpeRatioLines(Frontier, lty = 3, col = "orange")

## Feasible Portfolios:
   frontierPlot(Frontier, col = c("orange", "orange"), pch = 19)
   monteCarloPoints(Frontier, mcSteps = 5000, cex = 0.25, pch = 19)
   twoAssetsLines(Frontier, lwd = 2, col = "orange")
}


\keyword{models}

