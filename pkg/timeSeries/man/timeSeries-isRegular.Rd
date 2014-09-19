\name{isRegular}


\title{Checks if a time series is regular}


\alias{isDaily,timeSeries-method}
\alias{isMonthly,timeSeries-method}
\alias{isQuarterly,timeSeries-method}

\alias{isRegular,timeSeries-method}

\alias{frequency,timeSeries-method}


\description{

    Checks if a time series is regular. 
    
}


\details{

    What is a regular time series? If a series is a daily, a monthly, 
    or a weekly time series then we speak of a regular series.
    This can be tested calling the functions \code{isDaily}, 
    \code{isMonthly}, \code{isQuarterly}, or in general \code{isRegular}
    If the series is regular then the frequency of the series can 
    be determined calling the function \code{frequency}.

    A time series is defined as daily if the series has not more than 
    one date/time stamp per day.\cr
    A time series is defined as monthly if the series has not more than 
    one date/time stamp per month.\cr
    A time series is defined as quarterly if the series has not more than 
    one date/time stamp per quarter.\cr
    Note, amonthly series is also a daily series, a quarterly series is 
    alsona monthly series.\cr
    With these definitions a regular series is either a monthly or a 
    quarterly series.\cr
   
    NOT yet implemented is the case of weekly series.
  
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
        an R object of class 'timeSeries'.
    }
    \item{\dots}{
        arguments to be passed.
    }

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

