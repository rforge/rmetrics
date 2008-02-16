\name{fPORTFOLIO-class}


\alias{fPORTFOLIO}
\alias{fPORTFOLIO-class}
   
\alias{plot.fPORTFOLIO}
\alias{summary.fPORTFOLIO}


\title{Portfolio Class}


\description{
  
    A collection and description of functions 
    allowing to gain information about optimal 
    portfolios. Generally, optimization is done 
    via three arguments, data, specification of 
    the portfolio, and constraints, while function
    portfolioFrontier has two additional 
    arguments for title and description. 
    
}
    

\usage{
\method{plot}{fPORTFOLIO}(x, which = "ask", control = list(), \dots)

\method{summary}{fPORTFOLIO}(object, \dots)
}


\arguments{

    \item{control}{
        a list, defining the plotting parameters. The list modifies amongst
        others the color, e.g. \code{minvariance.col}, type of point, e.g.
        \code{tangency.pch}, or the dimension of the point, e.g. \code{cml.cex},
        see Notes for a complete list of control parameters.
        }
    \item{which}{
        which of the plots should be displayed? \code{which} can be either a 
        character string, \code{"all"} (displays all plots) or \code{"ask"} 
        (interactively asks which one to display), or a vector of integer
        values displaying the corresponding plot. Default value is 
        \code{"ask"}.
        }
    \item{object, x}{
        an S4 object of class \code{fPORTFOLIO}.
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }

}


\details{

    \bold{Portfolio Class:}
    \cr\cr
    
    This S4 class contains all information about the portfolio. Basically 
    these are risk measure, mean and covariance estimation, target return, 
    risk free rate, number of frontier points, ranges for calculation, see 
    the "Value" section  for a detailed description of the slots. 

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
        the portfolio specifications, see \code{PortfolioSpec}
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
    } % end describe  
                   
}


%\references{}


\examples{
## ---
}


\keyword{models}

