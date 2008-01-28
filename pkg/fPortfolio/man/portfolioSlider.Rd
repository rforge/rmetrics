\name{PortfolioSlider}

\alias{PortfolioSlider}

\alias{weightsSlider}     
\alias{frontierSlider}


\title{Portfolio Slider}


\description{
  
    Pop ups interactive plots onportfolio
    weights and the portfolio frontier.
    \cr
    
    The functions are:
    
    \tabular{ll}{           
    \code{frontierSlider} \tab starts the efficient frontier Slider, \cr
    \code{weightsSlider} \tab starts the weights slider. }
    
}
    

\usage{     
frontierSlider(object, control = list(), \dots)
weightsSlider(object, control = list(), \dots)
}



\arguments{

    \item{control}{
        a list, defining the plotting parameters. The list modifies amongst
        others the color, e.g. \code{minvariance.col}, type of point, e.g.
        \code{tangency.pch}, or the dimension of the point, e.g. \code{cml.cex},
         see Notes for a complete list of control parameters.
        }
    \item{object}{
         an S4 object of class \code{fPORTFOLIO}.
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }

}


\details{
  
    The sliders have illustrative objectives. The functions expect an S4 object
    of class fPortfolio.\cr
    
    \code{frontierSlider}: With the frontier slider it is possible to slide 
    over the efficient frontier. Risk and return of the current portfolio is 
    plotted in the title. The frontier slider has several addable plot features 
    listed in the following. Little pies at the current portfolio showing the 
    weights (left pie) and/or the attributed weights (right pie), legend, 
    minimum variance portfolio, tangency portfolio, market portfolio with 
    capital market line (risk free rate is slidable), Sharpe ratio plot, equal 
    weights portfolio, single asset portfolios, two assets short selling 
    efficient frontier, Monte Carlo portfolios (number is sliderable in a 
    range from 0  to 25000).
    \cr    
    
    \code{weightsSlider}: The weights slider gives an overview of the weights 
    on the efficient frontier. Three weight plots \code{weightsPlot}, 
    \code{piePlot} and the not stacked weights and a frontier plot with the
    single assets, the tangency portfolio and a legend are provided. In the 
    two weights plots the vertical line indicates the current portfolio and a 
    dotted one indicates the minimum variance portfolio. The number in the 
    pie plot stands for the asset and the sign shows whether this asset is 
    short or long. In all plots colors represent the same asset.

}


\value{
    
    Creates interactive plots.
    
}


\section{Control Parameters}{

    In the following all elements of argument control from functions
    \code{plot}, \code{weightsSlider}, \code{frontierSlider} are listed. 

    \describe{
    \item{sliderResolution}{
        [weightsSlider, frontierSlider] - 
        a numeric, determining the numbers of slider points, by default 
        nFrontierPoints/10.
        } 
    \item{sliderFlag}{
        [weightsSlider, frontierSlider] - 
        a character string, denoting the slidertype, by default "frontier"
        for \code{frontierSlider} and "weights" for \code{weightsSlider}.
        } 
    \item{sharpeRatio.col}{
        [plot, frontierSlider] - 
        a character string, defining color of the Sharpe
        ratio plot, by default "black".
        }
    \item{minvariance.col}{
        a character string, defining color of the minimum variance
        portfolio, by default "red".
        }
    \item{tangency.col}{
        a character string, defining color of the tangency
        portfolio, by default "steelblue".
        }
    \item{cml.col}{
        [plot, frontierSlider] - 
        a character string, defining  color of the market
        portfolio and the capital market line, by default "green".
        }
    \item{equalWeights.col}{
        [plot, frontierSlider] - 
        a character string, defining the color of the
        equal weights portfolio, by default "blue".
        }
    \item{runningPoint.col}{
        [weightsSlider] - 
        a character string, defining color of the
        point indicating the current portfolio, by default "red".
        }
    \item{singleAsset.col}{
        a character string vector, defining color of the
        single asset portfolios. The vector must have length the number 
        of assets, by default \code{rainbow}.
         }
    \item{twoAssets.col}{
        [plot, frontierSlider] - 
        a character string, defining color of the
         two assets efficient frontier, by default "grey".
         }
    \item{monteCarlo.col}{
        [plot, frontierSlider] - 
        a character string, defining color of the
        Monte Carlo portfolios, by default "black".
         }     
    \item{minvariance.pch}{
        a number, defining symbol used for the minimum
        variance portfolio. See \code{\link{points}} for description.
        Default symbol is 17.
        }
    \item{tangency.pch}{    
        a number, defining symbol used for the tangency portfolio.
        See \code{\link{points}} for description.
        Default symbol is 17.
        }
    \item{cml.pch}{
        [plot, frontierSlider] - 
        a number, defining symbol used for the market portfolio.
        See \code{\link{points}} for description.
        Default symbol is 17.
        }
    \item{equalWeights.pch}{
        [plot, frontierSlider] - 
        a number, defining symbol used for the equal weights portfolio.
        See \code{\link{points}} for description.
        Default symbol is 15.
        }        
    \item{singleAsset.pch}{
        a number, defining symbol used for the single asset portfolios.
        See \code{\link{points}} for description.
        Default symbol is 18.
        }           
    \item{sharpeRatio.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        Sharpe ratio plot, by default 0.1.
        }
    \item{minvariance.cex}{
        a number, determining size (percentage) of the
        minimum variance portfolio symbol, by default 1.
        }
    \item{tangency.cex}{
        a number, determining size (percentage) of the
        tangency portfolio symbol, by default 1.25.
        }
    \item{cml.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        market portfolio symbol, by default 1.25.
        }
    \item{equalWeights.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        equal weights portfolio symbol, by default 0.8.
        }
    \item{runningPoint.cex}{
        [weightsSlider] - 
        a number, determining size (percentage) of the
        point indicating the current portfolio
        equal weights portfolio symbol, by default 0.8.
        }
    \item{singleAsset.cex}{
        a number, determining size (percentage) of the
        singel asset portfolio symbols, by default 0.8.
        }
    \item{twoAssets.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        two assets efficient frontier plot, by default 0.01.
        }
    \item{monteCarlo.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        Monte Carol portfolio symbols, by default 0.01.
        }         
    \item{monteCarlo.cex}{
        [plot, frontierSlider] - 
        a number, determining size (percentage) of the
        Monte Carol portfolio symbols, by default 0.01.
        }
    \item{mcSteps}{
        [plot] - 
        a number, determining number of Monte Carol portfolio, by default 5000.
        }
    \item{pieR}{
        [plot, frontierSlider] - 
        a vector, containing factors for shrinking and stretching the x- and
        y-axis, by default NULL, i.e. c(1, 1) is used. Default pie size is 
        1/15 of the plot range.
        }
    \item{piePos}{
        [plot, frontierSlider] - 
        a number, determining the weight on the efficient frontier,
        which is illustrated by the pie. Default is tangency portfolio
        }
    \item{pieOffset}{
        [plot, frontierSlider] - 
        a vector, containing the pie's x- and y-axis offset from the efficient
        frontier. Default is NULL, i.e. the pie is set one default radius left
        of the efficient frontier.
        }
    \item{xlim}{
        [weightsSlider, frontierSlider] - 
        a vector, containing x-axis plot limits of the efficient frontier.
        Default setting is maximum of frontier range or single assets 
        portfolios.
        }
    \item{ylim}{
        [weightsSlider, frontierSlider] - 
        a vector, containing y-axis plot limits of the efficient frontier.
        Default setting is maximum of frontier range or single assets 
        portfolios. 
        } 
    }                 
}

%\references{}


\seealso{

   \code{portfolioClass},
   \code{portfolioData},
   \code{portfolioSpec},
   \code{portfolioConstraints},
   \code{portfolioPlots}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## minvariancePortfolio -
   # Load Data and Convert to timeSeries Object:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   # Set Default Specifications:
   Spec = portfolioSpec()
   Spec
   # Allow for unlimited Short Selling:
   Constraints = "Short"
   # Compute Short Selling Minimum Variance Portfolio
   minvariancePortfolio(Data, Spec, Constraints)
    
## portfolioFrontier -
   # Modify Constraints - Now Long Only Constraints:
   Constraint = c("minW[1:nAssets]=0")
   # Calculation of the Efficient Frontier
   frontier = portfolioFrontier(Data, Spec, Constraint)
   print(frontier)
    
## frontierSlider -
   # Try Frontier Slider:
   # frontierSlider(frontier)

## weightsSlider -
   # Try Weights Slider:
   # weightsSlider(frontier)
}


\keyword{models}

