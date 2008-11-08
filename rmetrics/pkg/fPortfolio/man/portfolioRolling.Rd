\name{portfolioRolling}


\alias{portfolioRolling}
\alias{rollingPortfolio}

\alias{rollingWindows}

\alias{rollingCmlPortfolio}
\alias{rollingTangencyPortfolio}
\alias{rollingMinvariancePortfolio}

\alias{rollingPortfolioFrontier}


\title{Rolling Portfolio}


\description{
  
    A collection and description of functions 
    allowing to roll a portfolio optimization
    over time.
    
    The functions are:
    
    \tabular{ll}{
    \code{rollingWindows} \tab Returns a list of rolling window frames, \cr
    \code{rollingCmlPortfolio} \tab Rolls a CML portfolio, \cr
    \code{rollingTangencyPortfolio} \tab Rolls a tangency portfolio, \cr
    \code{rollingMinvariancePortfolio} \tab Rolls a minimum risk portfolio, \cr
    \code{rollingPortfolioFrontier} \tab returns an efficient portfolio}
    
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
}



\arguments{

    \item{action}{
        a  character string naming a user defined function. This function
        is optionally applied after each rolling step.
        }
    \item{by}{
        a character string, by default \code{"1m"}, which denotes 1
        month. The shift by which the portfolio is rolled.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        a list, having a statistics named list, having named entries 'mu' and 
        'Sigma', containing the information of the statistics.
        }
    \item{description}{
        a character string, allowing for a brief project description,
        by default NULL, i.e. Date and User.
        }
    \item{from, to}{
        a vector of S4 \code{timeDate} objects which denote the starting
        and ending dates for the investigation.
        }
    \item{period}{
        a character string, by default \code{"12m"}, which denotes 12
        months. The period over which the portfolio is rolled.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}.
        }
    \item{title}{
        a character string, containing the title for the object, by 
        default NULL.
        }
    \item{x}{
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
    
}


%\references{}


\examples{
## ...
}


\keyword{models}

