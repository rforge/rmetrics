\name{weightsPie}


\alias{weightsPie}
\alias{attributesPie}
\alias{covRiskBudgetsPie}
\alias{tailRiskBudgetsPie}


\title{Portfolio Weights Pie Plots}


\description{

    Weights and related Pie Plots
    \cr

    The functions are:

    \tabular{ll}{
    \code{weightsPie} \tab Plots staggered weights, \cr
    \code{attributesPie} \tab Plots weighted means, \cr
    \code{covRiskBudgetPie} \tab Plots weighted risks, \cr
    \code{tailRiskBudgetPie} \tab Plots weighted risks.}

}


\usage{
weightsPie(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
attributesPie(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
covRiskBudgetsPie(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
tailRiskBudgetsPie(object, pos = NULL, col = NULL, box = TRUE, legend = TRUE)
}


\arguments{

    \item{box}{
        a logical value, determining whether a frame (box) should be plotted
        around the pie, by default TRUE.
        }
    \item{col}{
        a character string vector, setting the color. 
        }
    \item{legend}{
        a numeric value, determining the position on the efficient frontier 
        plotting the pie, by default NULL, i.e. expecting a object having 
        only one set of weights like the tangency portfolio.
        }
    \item{pos}{
        a logical value, determining whether a legend with the names of the
        assets should be plotted, by default TRUE.
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

