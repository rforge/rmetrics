\name{PortfolioPlots}

\alias{PortfolioPlots}

\alias{frontierPlot}

\alias{weightsPlot} 
\alias{weightsPie} 
\alias{attributesPlot}

\alias{covEllipsesPlot}


\title{Portfolio Plots}


\description{

    A collection and description of plot functions. 
    Included are weights, frontier, and related plots,
    as well as covariance ellipses plots.
    \cr

    The functions are:

    \tabular{ll}{
    \code{frontierPlot} \tab Plots efficient frontier, \cr
    \code{weightsPlot} \tab Plots staggered weights, \cr
    \code{weightsPie} \tab Plots staggered weights, \cr
    \code{attributesPlot} \tab Plots weighted means, \cr
    \code{covEllipsesPlot} \tab Plots covariance ellipses. }

}


\usage{
frontierPlot(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, \dots)
    
weightsPlot(object, col = NULL, legend = TRUE)
weightsPie(object, col = NULL, box = TRUE, legend = TRUE)
attributesPlot(object, col = NULL, legend = TRUE)
%attributesPie(object, col = NULL, legend = TRUE)
%riskBudgetPlot(object, col = NULL, legend = TRUE)
%riskBudgetPie(object, col = NULL, legend = TRUE)

covEllipsesPlot(x = list(), \dots)
}


\arguments{

    \item{add}{
        [frontierPlot] - \cr
        a logical value, determining whether the frontier should be added to 
        an existing plot, by default FALSE.
        }
    \item{box}{[weightsPie] - \cr
        a logical value, determining whether a frame (box) should be plotted
        around the pie, by default TRUE.
        }
    \item{col}{
        a character string vector, setting the color. For \code{frontierPlot} 
        it is two dimensional a vector; first entry is the upper part of the
        frontier,\cr second entry the lower, by default "black" and "grey".\cr
        For the other functions the argument defines the color representation,
        by default sets the default color is the rainbow palette.
        }
    \item{frontier}{
        [frontierPlot] - \cr
        a character string, determining which part of the frontier should be
        extracted. \code{"both"} stands for the full hyperbola, \code{"lower"}
        for all points below the minimum variance return and \code{"upper"}
        for the actual efficient frontier, by default "both".
         }
    \item{legend}{
        [weightsPlot, attributesPlot] - \cr
        a logical value, determining whether a legend with the names of the
        assets should be plotted, by default TRUE
        }
    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, containing slots call, data,
        specification, constraints, portfolio, title, description.
        }
    \item{x}{
        [covEllipsesPlot] - \cr
        a list of at least two covariance matrices.
        }
    \item{\dots}{
        [covEllipsesPlot] - \cr
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
    \cr


    \bold{Weights and Attributes Plots:}
    \cr\cr
    This two plots give a view on the weights of the whole frontier. A vertical
    dotted line indicates the minimum variance portfolio. The weights plot
    shows the stacked weights, while the attributes plot shows the
    mean weighted weights of the portfolios, i.e. the real return.
    \cr


    \bold{Covariance Ellipses Plot:}
    \cr\cr
    This plot visualizes the difference between two or more covariance matrices.
    It is meant to compare different methods of covariance estimation.

}



%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},\cr
   \code{\link{PortfolioExtractors}}.

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
   # Adding Lower Part to Plot
   frontierPlot(frontier, frontier = "lower", add = TRUE)
    
## weightsPlot -
   # View Weights Plot:
   weightsPlot(frontier)
    
## attributesPlot -
   # View Attributes Plot:
   attributesPlot(frontier, legend = TRUE)
}


\keyword{models}

