\name{PortfolioClass}

\alias{PortfolioClass}

\alias{fPORTFOLIO}
\alias{fPORTFOLIO-class}
\alias{fPortfolio}

\alias{show.fPORTFOLIO}
\alias{show,fPORTFOLIO-method}

\alias{feasiblePortfolio}
\alias{cmlPortfolio}
\alias{tangencyPortfolio}
\alias{minvariancePortfolio}
\alias{efficientPortfolio}
\alias{portfolioFrontier}
      
\alias{plot.fPORTFOLIO}
\alias{summary.fPORTFOLIO}

\alias{weightsSlider}     
\alias{frontierSlider}


\title{Portfolio Class}


\description{
  
    A collection and description of functions 
    allowing to gain information about optimal 
    portfolios. Generally, optimization is done 
    via three arguments, data, specification of 
    the portfolio, and constraints, while function
    portfolioFrontier() has two additional 
    arguments for title and description. 
    Additionally to the optimization functions 
    there are two sliders and a plot function 
    available.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{feasiblePortfolio} \tab Returns a feasible portfolio, \cr
    \code{cmlPortfolio} \tab returns the capital market line portfolio, \cr
    \code{tangencyPortfolio} \tab returns the tangency portfolio, \cr
    \code{minvariancePortfolio} \tab returns the minimum variance portfolio, \cr
    \code{efficientPortfolio} \tab returns an efficient portfolio, \cr
    \code{portfolioFrontier} \tab returns the efficient frontier, \cr
    \code{show} \tab print method for 'fPPORTFOLIO' objects, \cr   
    \code{plot} \tab plot method for objects of class fPORTFOLIO, \cr   
    \code{weightsSlider} \tab starts the weights slider, \cr           
    \code{frontierSlider} \tab starts the efficient frontier Slider. }
    
}
    

\usage{
portfolioFrontier(data, spec = portfolioSpec(), constraints = NULL, 
    title = NULL, description = NULL)

feasiblePortfolio(data, spec = portfolioSpec(), constraints = NULL)
cmlPortfolio(data, spec = portfolioSpec(), constraints = NULL)
tangencyPortfolio(data, spec = portfolioSpec(), constraints = NULL)
minvariancePortfolio(data, spec = portfolioSpec(), constraints = NULL)
efficientPortfolio(data, spec = portfolioSpec(), constraints = NULL)

show.fPORTFOLIO(object)
\method{plot}{fPORTFOLIO}(x, which = "ask", control = list(), \dots)
\method{summary}{fPORTFOLIO}(object)

weightsSlider(object, control = list(), \dots)     
frontierSlider(object, control = list(), \dots)
}



\arguments{

    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{control}{
        [plot, *Sliders] - \cr
        a list, defining the plotting parameters. The list modifies amongst
        others the color, e.g. \code{minvariance.col}, type of point, e.g.
        \code{tangency.pch}, or the dimension of the point, e.g. \code{cml.cex},
         see Notes for a complete list of control parameters.
        }
    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{description}{
        [portfolioFrontier] - \cr
        a character string, allowing for a brief project description,
        by default NULL, i.e. Date and User.
        }
    \item{object}{
        [show, *Sliders] - \cr
         an S4 object of class \code{fPORTFOLIO}, containing slots call, data,
         specification, constraints, portfolio, title, description, see Value 
         for a full slot description.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, containing slots call, model,
        portfolio, title, description, see \code{\link{PortfolioSpecification}}
        for a full slot description.
        }
    \item{title}{
        [portfolioFrontier] - \cr
        a character string, containing the title for the object, by default NULL.
        }
    \item{which}{
        [plot] - \cr
        which of the plots should be displayed? \code{which} can be either a 
        character string, \code{"all"} (displays all plots) or \code{"ask"} 
        (interactively asks which one to display), or a vector of integer
        values displaying the corresponding plot. Default value is 
        \code{"ask"}.
        }
    \item{x}{
        [plot] - \cr
        an S4 object of class \code{fPORTFOLIO}.
        }
    \item{\dots}{
        [plot, *Sliders] - \cr
        optional arguments to be passed.\cr
        }

}


\details{

    \bold{Portfolio Class:}
    \cr\cr
    This S4 class contains all information about the portfolio. Basically this
    are risk measure, mean and covariance estimation, target return, risk free
    rate, number of frontier points, ranges for calculation, see the "Value"
    section  for a detailed description of the slots. 
    \cr
    
    
    \bold{Portfolio Frontier:}
    \cr\cr
    The function \code{portfolioFrontier} calculates the whole efficient frontier.
    The portfolio information consists of five arguments: data, specifications,
    constraints, title and description. The data can be either the mean vector
    and the covariance matrix in a equivalently named list or a time series
    table. The specifications are done by the \code{portfolioSpec} function,
    see \code{\link{PortfolioSpecification}}. The constrains have the following
    syntax. For bound constraints: \code{"minW[asset]=percentage"} and for
    sector constraints \code{"maxsumW[assets]=percentage"}. The function chooses
    from this information the right algorithm to calculate the efficient frontier.
    The function returns an S4 object of class \code{fPortfolio} containing
    amongst others risk, return and weights of the whole efficient frontier,
    see Value for further details.
    \cr
    
    \bold{Individual Portfolios:}
    \cr\cr   
    Individual portfolio functions allow a direct computation of portfolios 
    with specific properties. The functions are:
    
    \tabular{ll}{
    \code{feasiblePortfolio} \tab a feasible portfolio, \cr
    \code{cmlPortfolio} \tab the capital market line portfolio, \cr
    \code{tangencyPortfolio} \tab the tangency portfolio, \cr
    \code{minvariancePortfolio} \tab the minimum variance portfolio, \cr
    \code{efficientPortfolio} \tab an efficient portfolio. } 
    
    The functions require three arguments: \code{data}, \code{spec} 
    (specifications), and \code{constraints}, see above. The functions 
    are named according to their specific property and return the result
    of the computed or optimized portfolio as an S4 object of class 
    \code{fPortfolio}.
    
    \emph{FeasiblePortfolio:} The weights are retrieved from the 
    specification structure, by default they are undefined, \code{NULL},
    and in this case the function \code{feasiblePortfolio} assumes
    equal weights. To set user specified weights in the specification 
    structure, use the function \code{setWeights}. 
    
    \bold{Portfolio Sliders:}
    \cr\cr   
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
    
    \code{portfolioFrontier()}\cr
    returns an S4 object of class \code{"fPORTFOLIO"}, with the following 
    slots:

    \item{@call}{
        a call, returning the matched function call.
        }
    \item{@data}{
        a list with two named elements, \code{series} holding the time series
        data if available, otherwise NA, and \code{statistics}, itself a
        named list with two named elements \code{mu} and \code{Sigma}
        holding the vector of means and the matrix of covariances.
        }  
    \item{@description}{
        a character string, allowing for a brief project description.
        }        
    \item{@portfolio}{
        a list, containing parameter specifications for the portfolio:\cr 
        \code{weights} a numeric vector specifying the portfolio 
            weights,\cr
        \code{targetReturn} a numeric value specifying the target 
            return,\cr
        \code{targetRisk} a numeric value specifying the target 
            risk,\cr  
        \code{targetMean} a numeric value specifying the target 
            return determinated with function mean(),\cr
        \code{targetStdev} a numeric value specifying the target risk in 
            standart deviation as risk measure.
        }
    \item{@specification}{
        a list with one named element \code{spec} which represents an object 
        of class \code{fPFOLIOSPEC}, including all information about
        the portfolio specifications, see \code{\link{PortfolioSpecification}}
        for further details.
        }
    \item{@title}{
        a title string.
        }
    
    \code{feasiblePortfolio}\cr
    \code{cmlPortfolio}\cr
    \code{tangencyPortfolio}\cr
    \code{minvariancePortfolio}\cr
    \code{efficientPortfolio}\cr
    return an S4 object of class \code{fPORTFOLIO} having information only 
    about one portfolio.
    
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

   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{PortfolioConstraints}},
   \code{\link{PortfolioExtractors}},
   \code{\link{PortfolioPlots}}.

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
    
## plot -
   # Plot Efficient Frontier with Minimum Variance Portfolio
   plot(frontier, which = c(1, 3))
    
## frontierSlider -
   # Try Frontier Slider:
   # frontierSlider(frontier)

## weightsSlider -
   # Try Weights Slider:
   # weightsSlider(frontier)
}


\keyword{models}

