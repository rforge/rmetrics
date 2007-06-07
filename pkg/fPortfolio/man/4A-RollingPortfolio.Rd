\name{RollingPortfolio}

\alias{RollingPortfolio}

\alias{rollingWindows}

\alias{rollingCmlPortfolio}
\alias{rollingTangencyPortfolio}
\alias{rollingMinvariancePortfolio}

\alias{rollingPortfolioFrontier}

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
    \code{portfolioBacktesting} \tab Does portfolio backtesting. }
    
}
    

\usage{
rollingWindows(x, period = "12m", by = "1m")

rollingCmlPortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, \dots)
rollingTangencyPortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, \dots)
rollingMinvariancePortfolio(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, \dots)
    
rollingPortfolioFrontier(data, spec, constraints, from, to, action = NULL, 
    title = NULL, description = NULL, \dots)
   
portfolioBacktesting(formula, data, spec = portfolioSpec(), constraints = NULL, 
    portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m", 
    trace = TRUE)
}



\arguments{

    \item{action}{
        [*Portfolio][*Frontier] - \cr
        a  character string naming a user defined function. This function
        is optionally applied after each rolling step.
        }
    \item{by}{
        [rollingWindows] - \cr
        a character string, by default \code{"1m"}, which denotes 1
        month. The shift by which the portfolio is rolled.
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
        }
    \item{description}{
        [*Portfolio][*Frontier] - \cr
        a character string, allowing for a brief project description,
        by default NULL, i.e. Date and User.
        }
    \item{formula}{
        [*Backtesting] - \cr
        a formula describing the benchmark and assets used for backtesting
        in the form \code{backtest ~ assetA + ... + assetZ}. Here,
        \code{backtest} and \code{asset*} are column names of the
        \code{data} set.
        }
    \item{from, to}{
        [*Portfolio][*Frontier] - \cr
        a vector of S4 \code{timeDate} objects which denote the starting
        and ending dates for the investigation.
        }
    \item{horizon}{
        [*Backtesting] - \cr
        a character string, by default \code{"12m"}, which denotes 12
        months. The period over which the portfolio is rolled.
        }
    \item{period}{
        [rollingWindows] - \cr
        a character string, by default \code{"12m"}, which denotes 12
        months. The period over which the portfolio is rolled.
        }
    \item{portfolio}{
        [portfolioBacktesting] - \cr
        a character string, the function name of a user defined 
        portfolio function, which defines the rolling portfolio
        strategy.
        }
    \item{smoothing}{
        [portfolioBacktesting] - \cr
        a character string, by default \code{"6m"}, which denotes 6
        months. The period over which the rolling weights are smoothed
        by an exponential moving average.
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
        [portfolioBacktesting][*OptimalPortfolio] - \cr
        a logical value. Should the calculataion be traced?
        }
    \item{x}{
        [rollingWindows] - \cr
        an S4 object of class \code{timeSeries} from which the rolling
        window frames will be created. The length of these frames is given
        by the argument \code{period} and they are shifted by the value
        specified by the argument \code{by}.
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
    
    
    \bold{Rolling Backtesting:}
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
    
    \code{portfolioBacktesting}\cr
    returns ...
    
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

