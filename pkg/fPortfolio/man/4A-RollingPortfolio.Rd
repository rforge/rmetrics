\name{RollingPortfolio}

\alias{RollingPortfolio}

\alias{rollingWindows}

\alias{rollingCmlPortfolio}
\alias{rollingTangencyPortfolio}
\alias{rollingMinvariancePortfolio}

\alias{rollingPortfolioFrontier}

\alias{rollingBacktestPortfolio}
\alias{portfolioBacktesting}
\alias{portfolioBacktestingStats}


\title{Rolling Portfolio}


\description{
  
    A collection and description of functions 
    allowing to roll a portfolio optimization
    over time.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{rollingWindows} \tab Returns a list of rolling window frames, \cr
    \code{rollingCmlPortfolio} \tab Rolls a CML portfolio, \cr
    \code{rollingTangencyPortfolio} \tab Rolls a tangency portfolio, \cr
    \code{rollingMinvariancePortfolio} \tab Rolls a minimum risk portfolio, \cr
    \code{rollingPortfolioFrontier} \tab returns an efficient portfolio, \cr
    \code{rollingBacktestPortfolio} \tab Rolls an optimal portfolio, \cr 
    \code{portfolioBacktesting} \tab Does portfolio backtesting, \cr  
    \code{portfolioBacktestingStats} \tab pComputes monthly portfolio statistics. }
    
}
    

\usage{
rollingWindows(x, period = "24m", by = "1m")

rollingCmlPortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, ...)
rollingTangencyPortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, ...)
rollingMinvariancePortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, ...)
    
rollingPortfolioFrontier(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, ...)
   
rollingBacktestPortfolio(data, spec, constraints, from, to, benchmark, 
    action = NULL, trace = TRUE, title = NULL, description = NULL, ...) 
portfolioBacktesting(formula, data, horizon, smoothing, trace = TRUE)
portfolioBacktestingStats(x)
}



\arguments{

    \item{action}{
        [*Portfolio][*Frontier] - \cr
        .
        }
    \item{benchmark}{
        [rollingOptimalPortfolio] - \cr
        .
        }
    \item{by}{
        [rollingWindows] - \cr
        .
        }
    \item{constraints}{
        [*Portfolio][*Frontier] - \cr
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        [*Portfolio][*Frontier] - \cr
        a list, having a statistics named list, having named entries 'mu' and 
        'Sigma', containing the information of the statistics. \cr
        [*Backtesting] - \cr
        .
        }
    \item{description}{
        [*Portfolio][*Frontier] - \cr
        a character string, allowing for a brief project description,
        by default NULL, i.e. Date and User.
        }
    \item{formula}{
        [*Backtesting] - \cr
        .
        }
    \item{from, to}{
        [*Portfolio][*Frontier] - \cr
        .
        }
    \item{horizon}{
        [*Backtesting] - \cr
        .
        }
    \item{period}{
        [rollingWindows] - \cr
        .
        }
    \item{smoothing}{
        [*Backtesting] - \cr
        .
        }
    \item{spec}{
        [*Portfolio][*Frontier] - \cr
        an S4 object of class \code{fPFOLIOSPEC}, containing slots call, 
        model, portfolio, title, description, 
        see \code{\link{PortfolioSpecification}}
        for a full slot description.
        }
    \item{title}{
        [*Portfolio][*Frontier] - \cr
        a character string, containing the title for the object, by 
        default NULL.
        }
    \item{trace}{
        [*Backtesting][*OptimalPortfolio] - \cr
        .
        }
    \item{x}{
        [rollingWindows] - \cr
        an S4 object of class \code{timeSeries} from which the rolling
        window frames will be created. The length of these frames is given
        by the argument \code{period} and they are shifted by the value
        specified by the argument \code{by}. \cr
        [portfolioBacktestingStats] - \cr
        an object as returned by the function portfolioBacktesting
        
        }
    \item{\dots}{
        optional arguments to be passed.\cr
        }   

}


\details{

    \bold{RollingWindows:}
    The function \code{rollingWindows} constructs from a 'timeSeries'
    object windows frames of given length \code{period} and shift
    \code{by}. ...
    \cr
    
    
    \bold{Rolling Portfolios:}
    \cr\cr
    The functions \code{rolling*Portfolio} ...
    \cr
    
    
    \bold{Rolling Frontier:}
    \cr\cr   
    The function \code{rollingPortfolioFrontier} ...
    
    
    \bold{Backtesting:}
    \cr\cr   
    The functions  ...

}


\value{
    
    \code{rollingwindows()}\cr
    returns ...
    \cr
    
    \code{rollingCmlPortfolio}\cr
    \code{rollingTangencyPortfolio}\cr
    \code{rollingMinvariancePortfolio}\cr
    return ...
    \cr
    
    \code{rollingPortfolioFrontier}\cr
    returns ...
    \cr
    
    \code{rollingBacktestPortfolio}\cr
    returns ...
    \cr
    
    \code{portfolioBacktesting}\cr
    returns ...
    \cr
    
    \code{portfolioBacktestingStats}\cr
    returns ...
    \cr
    
}


%\references{}


\seealso{

   \code{\link{PortfolioClass}},
   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{PortfolioConstraints}},
   \code{\link{PortfolioExtractors}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## ...
}


\keyword{models}

