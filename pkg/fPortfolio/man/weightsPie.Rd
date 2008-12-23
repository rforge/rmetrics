\name{weightsPie}


\alias{weightsPie}
\alias{weightedReturnsPie}
\alias{covRiskBudgetsPie}
\alias{tailRiskBudgetsPie}


\title{Portfolio Pie Plots}


\description{

    Displays pie plots of weights, weighted Returns, covariance 
    and tail risk budgets for a portfolio.

}


\usage{
weightsPie(object, pos = NULL, labels = TRUE, col = NULL, 
    box = TRUE, legend = TRUE, radius = 0.8, \dots)
    
weightedReturnsPie(object, pos = NULL, labels = TRUE, col = NULL, 
    box = TRUE, legend = TRUE, radius = 0.8, \dots)
    
covRiskBudgetsPie(object, pos = NULL, labels = TRUE, col = NULL, 
    box = TRUE, legend = TRUE, radius = 0.8, \dots)
    
tailRiskBudgetsPie(object, pos = NULL, labels = TRUE, col = NULL, 
    box = TRUE, legend = TRUE, radius = 0.8, \dots)
}


\arguments{

    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, as returned by one of
        the portfolio functions, e.g. \code{efficientPortfolio} or
        \code{portfolioFrontier}.
        }
    \item{pos}{
        NULL or an integer value. If NULL it is assumend that we consider
        a single portfolio like for example a tengency portfolio. However,
        if the \code{object} describes a whole frontier then \code{pos}
        has to be the number of that point from the frontier which we
        want to display. The frontier points are numbered from one up
        to the value give by the number of frontier points, which can
        be retrieved by calling \code{getNFrontierPoints}.
        }
    \item{labels}{
        a logical flag, determining if the graph should be labeled
        automatically, which is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself. 
        Evenmore, if \code{labels} takes the value of a string vector, 
        then the names of the assets from the porftolio \code{object} 
        will be ignored, and the labels will be taken from the specified 
        string vector.
        }
    \item{col}{
        a character string vector, defined from a color palette. The 
        default setting uses the "Blues" \code{seqPalette} palette.
        }
    \item{box}{
        a logical flag, determining whether a boxed frame should be plotted
        around the pie, by default the value is set to \code{TRUE}.
        }
    \item{legend}{
        a logical flag, determining if a legend should be added to the 
        plot. The default setting shows the legend.
        }
    \item{radius}{
        a numeric value, determining the radius of the pie. The default
        value is 0.8.
        }
    \item{\dots}{
        arguments to be passed.
        }
}


\details{

    The pie plots allow for different views on the results obtained
    from a feasible or an optimized portfolio.
    
    The function \code{weightsPie} displays the weights composition
    of a portfolio. 
    
    The function \code{weightedReturnsPie} displays the investment, i.e.
    the weighted returns of a portfolio. 
    
    The function \code{covRiskBudgetsPie} displays the covariance risk
    budgets of a portfolio.

    The function \code{taikRiskBudgetsPie} displays the copulae tail
    risk budgets of a portfolio. Note, this is only possible if in the
    portfolio specificsation a copulae tail risk is defined.

}


\note{

    With Rmetrics Version 2.7 the name of \code{attributesPie} has been
    changed to \code{weightedReturnsPie} since it better describes what
    the plot shows.
    
}



%\references{}


\seealso{

    \code{\link{weightsPlot}}, 
    \code{link{weightsSlider}}.
    
}


\examples{
## data -
   Data = SMALLCAP.RET
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]

## tangencyPortfolio - 
   portfolio = tangencyPortfolio(Data)
    
## weightsPie -
   weightsPie(portfolio)

## portfolioFrontier - 
   frontier = portfolioFrontier(Data)
   
## weightsPie -
   weightsPie(frontier, pos = 40)
}


\keyword{models}

