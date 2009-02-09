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

\alias{tailoredFrontierPlot}


\title{Efficient Frontier Plot}


\description{

    Plots the efficient frontier of an 
    optimized portfolio and allows to add
    points and lines from specif portfolios
    
    The functions are:
    
    \tabular{ll}{
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
    \code{monteCarloPoints} \tab Adds randomly produced feasible portfolios, \cr
    \code{tailoredFrontierPlot} \tab an example for a tailored plot. }

}


\usage{
frontierPlot(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, labels = TRUE,
    return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
    auto = TRUE, title = TRUE, \dots) 

minvariancePoints(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
cmlPoints(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
cmlLines(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
tangencyPoints(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
tangencyLines(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
equalWeightsPoints(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
singleAssetPoints(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
twoAssetsLines(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)
sharpeRatioLines(object, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)

monteCarloPoints(object, mcSteps = 5000, return = c("mean", "mu"), 
    risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE, \dots)

tailoredFrontierPlot(object, risk = c("Cov", "Sigma", "CVaR", "VaR"), 
    mText = NULL, col = NULL, xlim = NULL, ylim = NULL, 
    twoAssets = FALSE)
}


\arguments{

    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, containing slots call, data,
        specification, constraints, portfolio, title, description.
        }
    \item{frontier}{
        a character string, determining which part of the frontier should be
        extracted. \code{"both"} stands for the full hyperbola, \code{"lower"}
        for all points below the minimum variance return and \code{"upper"}
        for the actual efficient frontier, by default "both".
        }
    \item{col}{
        a character string vector, setting the color. For \code{frontierPlot} 
        it is a two dimensional a vector; first entry is the upper part of the
        frontier,\cr second entry the lower, by default "black" and "grey".\cr
        For the other functions the argument defines the color representation,
        by default sets the default color is the rainbow palette.
        }          
    \item{add}{
        a logical value, determining whether the frontier should be added to 
        an existing plot, by default FALSE.
        }
    \item{return}{
        a character string denoting which type of return should be
        plotted. Allowed values for the
        return are either \code{"mean"}, or \code{"mu"}.
        }
    \item{risk}{
        a character string denoting which type of risk should be
        plotted. Allowed values for the
        risk measure are either \code{"cov"}, \code{"sigma"},  
        \code{"VaR"}, or \code{"CVaR"}.
        }
    \item{auto}{
        a logical flag denoting if the type of return and risk to be
        plotted should be selected automatically, by default TRUE.
        }
    \item{labels}{
        a logical flag, should the plot be automatically labeled and
        decorated? By default \code{TRUE}.
        }
    \item{title}{
        a logical flag, should the plot obtain a default main title and
        x- and y-labels? By default \code{TRUE}.
        }
    \item{mcSteps}{
        an integer value, the number of Monte Carlo steps.
        }
    \item{xlim, ylim}{
        two numeric vectors with two elelemts , the plot range. 
        If set to NULL the values for the plot ranges are determined 
        automatically.
        }
    \item{mText}{
        a character string, representing a marginal text string. 
        If set to NULL the value is taken from the title of the
        input frontier argument.
        }
    \item{twoAssets}{
        a logical flag, if TRUE, then the two assets frontier lines
        will be drawn.
        }
    \item{\dots}{
        optional arguments to be passed.
        }

}


\details{

    \bold{Frontier Plot:}\cr
    
    The \code{frontierPlot} function allows to plot the efficient
    frontier. 
    
    \emph{Selecting the upper and/or lower part of the frontier:}
    
    One can choose one from three different plot options selectable 
    by the argument \code{frontier}. 
    The part superior to the minimum variance portfolio is plotted with 
    argument \code{frontier="upper"}. 
    The part below is plotted with \code{frontier="lower"}. 
    Both parts are plotted with \code{frontier="both"}, which is default. 
    
    \emph{Selecting the color of the frontier points:}
    
    Different colors for the lower and upper part are selected from the
    argument \code{col}, the defaults are grey and black, the first for
    the lower part of the frontier, thye second for the upper part.
    
    \emph{Plotting the frontiers for several portfolios in one plot:}
    
    The function comes with an argument \code{add}. If \code{add=FALSE}
    then a new plot is opended displaying the frontier for the selected 
    portfolio. On the other hand setting \code{add=TRUE} allows to add
    new efficient frontiers from other portfolios to the same plot.
    
    \emph{Removing labels from the plot:}
    By default we have \code{labels=TRUE} and thus additional information
    is plotted on the third side of the graph. To discard this information
    one has to set \code{labels = FALSE}.
    
    \emph{Adding user supplied title and axis-labels:}
    By default we have \code{title=TRUE} and thus a default main title and 
    default x- and y-labels are automatically added. To discard this
    kind of decoration set \code{title=FALSE}. Then using the function
    \code{title} allows to add user specified titles and axis-labels. 
    
    \emph{The dot arguments:}
    With the \code{\dots} arguments one can pass additional arguments to
    the internal plot and points functions.

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## data -
   # Load Data:
   Data = SMALLCAP.RET
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
   sharpeRatioLines(Frontier, col = "orange", lwd = 2)

## Feasible Portfolios:
   frontierPlot(Frontier, col = c("orange", "orange"), pch = 19)
   monteCarloPoints(Frontier, mcSteps = 5000, cex = 0.25, pch = 19)
   twoAssetsLines(Frontier, lwd = 2, col = "orange")
}


\keyword{models}

