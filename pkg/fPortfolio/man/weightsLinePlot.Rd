\name{weightsLinePlot}


\alias{weightsLinePlot}

\alias{weightedReturnsLinePlot}
\alias{covRiskBudgetsLinePlot}
%\alias{tailRiskBudgetsLinePlot}


\title{Portfolio Weights Line Plots}


\description{

    Displays line plots of weights, weighted returns, covariance 
    and tail risk budgets.

}


\usage{   
weightsLinePlot(object, labels = TRUE, col = NULL, title = TRUE, 
    box = TRUE, legend = TRUE, ...)
    
weightedReturnsLinePlot(object, labels = TRUE, col = NULL, title = TRUE, 
    box = TRUE, legend = TRUE, ...)
    
covRiskBudgetsLinePlot(object, labels = TRUE, col = NULL, title = TRUE, 
    box = TRUE, legend = TRUE, ...)
% NYI   
%tailRiskBudgetsLinePlot(object, labels = TRUE, col = NULL, title = TRUE, 
%    box = TRUE, legend = TRUE, ...)
}


\arguments{

    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, as returned by one of
        the portfolio functions, e.g. \code{efficientPortfolio} or
        \code{portfolioFrontier}.
        }
    \item{labels}{
        a logical flag, determining if the the graph should be labeled
        automatically, which is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself.
        }
    \item{col}{
        a character string vector, defined from a color palette. The 
        default setting uses the "Blues" \code{seqPalette} palette.
        }
    \item{title}{
        a logical flag. Should automatically a title and axis labels be
        added to the plot. 
        }
    \item{box}{
        a logical flag, determining whether a boxed frame should be plotted
        around the pie, by default the value is set to \code{TRUE}.
        }
    \item{legend}{
        a logical value, determining if the the graph should be labeled
        automatically, shich is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself. 
        Evenmore, if \code{labels} takes the value of a string vector, 
        then the names of the assets from the porftolio \code{object} 
        will be ignored, and the labels will be taken from the specified 
        string vector.
        }
    \item{\dots}{
        additional arguments passed to the function \code{barplot}. Only 
        active if \code{labels=FALSE}. 
        }
    

}


\details{

    These line plots allow for different views on the results obtained
    from a feasible or an optimized portfolio.
    
    The function \code{weightsPlot} displays the weights composition
    along the frontier of a portfolio. 
    
    The function \code{weightedReturnsPlot} displays the investment
    composition, i.e. the weighted returns along the frontier of a portfolio. 
    
    The function \code{covRiskBudgetsPlot} displays the covariance risk
    budgets composition along the frontier of a portfolio.

    % NYI
    %The function \code{tailRiskBudgetsPlot} displays the copulae tail
    %risk budgets composition along the frontier of a portfolio. Note, 
    %this is only possible if in the portfolio specification a copulae 
    %tail risk is defined.

}


\note{

    With Rmetrics Version 2.7 the name of \code{attributesPlot} has been
    changed to \code{weightedReturnsPlot} since it better describes what
    the plot shows.
    
    The axes labeling has been changed in all plots, now the return on the
    bottom side, and the risk on the upper side, before it was just vice
    versa.
    
}


%\references{}


\seealso{

    \code{\link{weightsPie}}, 
    \code{\link{weightsSlider}}.
    
}


\examples{
## data -
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]

## portfolioFrontier -
   # Compute Long Only Efficient Frontier
   frontier = portfolioFrontier(Data)
   
## weightsLinePlot -
   # View Weights Line Plot:
   # weightsLinePlot(frontier)
}


\keyword{models}

