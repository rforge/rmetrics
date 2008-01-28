\name{frontierPlot}


\alias{frontierPlot}


\title{Efficient Frontier Plot}


\description{

    Plots the efficient frontier of an 
    optimized portfolio.

}


\usage{
frontierPlot(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, \dots)
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


\seealso{

   \code{portfolioData},
   \code{portfolioSpec},
   \code{portfolioConstraints},
   \code{fPORTFOLIO}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.

}


\examples{
## portfolioFrontier -
   
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
   # Compute Long Only Efficient Frontier
   frontier = portfolioFrontier(Data)
   frontier
   
## frontierPlot -  
   
   # Plot Upper Part of Frontier:
   frontierPlot(frontier, frontier = "upper")
}


\keyword{models}

