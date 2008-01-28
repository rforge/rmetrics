\name{weightsPlot}


\alias{weightsPlot}
\alias{attributesPlot}
\alias{covRiskBudgetsPlot}
\alias{tailRiskBudgetsPlot}


\title{Portfolio Weights Plots}


\description{

    Weights and related plots.
    \cr

    The functions are:

    \tabular{ll}{
    \code{frontierPlot} \tab Plots efficient frontier, \cr
    \code{weightsPlot} \tab Plots staggered weights, \cr
    \code{attributesPlot} \tab Plots weighted means, \cr
    \code{covRiskBudgetsPlot} \tab  Plots covariance risk budgets, \cr
    \code{tailRiskBudgetsPlot} \tab  Plots tail risk budgets.}

}


\usage{   
weightsPlot(object, col = NULL, legend = TRUE)
attributesPlot(object, col = NULL, legend = TRUE)
covRiskBudgetsPlot(object, col = NULL, legend = TRUE)
tailRiskBudgetsPlot(object, col = NULL, legend = TRUE)
}


\arguments{

    \item{col}{
        a character string vector, setting the color. 
        }
    \item{legend}{
        a numeric value, determining the position on the efficient frontier 
        plotting the pie, by default NULL, i.e. expecting a object having 
        only one set of weights like the tangency portfolio.
        }
    \item{object}{
        an S4 object of class \code{fPORTFOLIO}, containing slots call, data,
        specification, constraints, portfolio, title, description.
        }

}


\details{

    \bold{Weights, Attributes, and Risk Budgets Plots:}
    \cr\cr
    
    These plotds give a view on three different views related to the weights 
    of the frontier. This concern the constitution of the weights, the 
    weights attributes and of the risk budgets. A vertical line indicates 
    the minimum variance portfolio. The weights plot displays staggered 
    weights, while the attributes plot shows the mean return weighted weights 
    of the portfolio, i.e. the real return. The risk budget plot shows the 
    risk weighted weights.

}



%\references{}


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
   # frontierPlot(frontier, frontier = "upper")
   # Adding Lower Part to Plot
   # frontierPlot(frontier, frontier = "lower", add = TRUE)
    
## weightsPlot -
   # View Weights Plot:
   # weightsPlot(frontier)
    
## attributesPlot -
   # View Attributes Plot:
   # attributesPlot(frontier, legend = TRUE)
}


\keyword{models}

