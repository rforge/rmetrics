\name{isRegular}


\title{Checks if a time series is regular}


\alias{isDaily,timeSeries-method}
\alias{isMonthly,timeSeries-method}
\alias{isQuarterly,timeSeries-method}

\alias{isRegular,timeSeries-method}

\alias{frequency,timeSeries-method}


\description{

    Checks if a time series is regular. i.e. if a series is 
    a daily, a monthly, or a weekly time series. If the series
    is regular the frequency of the serie scan determined
    calling the function frequency.
  
}


\usage{
\S4method{isDaily}{timeSeries}(x)
\S4method{isMonthly}{timeSeries}(x)
\S4method{isQuarterly}{timeSeries}(x)

\S4method{isRegular}{timeSeries}(x)

\S4method{frequency}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{  
        an R object of class \code{timeSeries}.
    }
    \item{\dots}{
        arguments to be passed.
    }
  
}


\details{

    A time series is defined as daily if the series has not more than 
    one date/time stamp per day.
    
    A time series is defined as monthly if the series has not more than 
    one date/time stamp per month.
    
    A time series is defined as quarterly if the series has not more than 
    one date/time stamp per quarter.
    
    A monthly series is also a daily series, a quarterly series is also
    a monthly series.
    
    A regular series is either a monthly or a quarterly series.
    
    NOT yet implemented is the case of weekly series.
    
}


\value{

    The \code{is*} functions return \code{TRUE} or \code{FALSE} depending 
    on whether the series fulfills the condition or not.
    
    The function frequency returns in general 1, for quarterly series 4,
    and for monthly series 12.
  
}


\examples{
## None
}


\keyword{chron}

