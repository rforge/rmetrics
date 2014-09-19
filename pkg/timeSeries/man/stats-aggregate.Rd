\name{aggregate-methods}

\docType{methods}

\alias{aggregate}
\alias{aggregate.timeSeries}
\alias{aggregate-methods}
\alias{aggregate,timeSeries-method}

\alias{daily2weekly}
\alias{daily2monthly}


\title{timeSeries Class, Functions and Methods}

\description{
    
    Aggregates a 'timeSeries' Object.      
    
}


\details{

    The function \code{aggregate} is a function which can 
    aggregate time series on general aggregation periods.
    
    In addition there are two tailored function for simple
    usage: Function \code{daily2monthly} and \code{daily2weekly}
    which allow to aggregate 'timeSeries' objects from daily
    to monthly or weekly levels, respectively.
    
    In the case of the function \code{daily2weekly} one can 
    explicitely the starting day of the week, the default
    value is Tuesday, \code{startOn="Tue"}.
}


\usage{
\S4method{aggregate}{timeSeries}(x, by, FUN, \dots)

daily2monthly(x, init=FALSE) 
daily2weekly(x, startOn="Tue", init=FALSE)
}


\arguments{
  
    \item{x}{
        an object of class 'timeSeries'.
        } 
    \item{by}{
        a sequence of \code{timeDate} objects denoting the aggregation
        period.
        }
    \item{FUN}{
        the function to be applied.
        }               
    \item{startOn}{
        a string value, specifying the day of week as a three
        letter abbreviation. Weekly aggregated data records
        are then fixed to the weekdays given by the argument
        \code{startOn}.}
    \item{init}{
        a logical value, if set to \code{TRUE} then the time 
        series will be indexed to 1 for its first value. By 
        default init is set to \code{FALSE}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{aggregate} returns an aggregated S4 object of class 
    \code{timeSeries}.
    
    \code{daily2monthly} returns an aggregated monthly object
    of class \code{timeSeries}.

    \code{daily2weekly} returns an aggregated weekly object
    of class \code{timeSeries} starting on the specified day
    of week.
        
}


\examples{
## Load Microsoft Data Set -
   x <- MSFT

## Aggregate by Weeks - 
   by <- timeSequence(from = start(x),  to = end(x), by = "week")
   aggregate(x, by, mean)

## Aggregate to Last Friday of Month -
   by <- unique(timeLastNdayInMonth(time(x), 5))
   X <- aggregate(x, by, mean)
   X
   dayOfWeek(time(X))
   isMonthly(X)

## Aggregate to Last Day of Quarter -
   by <- unique(timeLastDayInQuarter(time(x)))
   X <-  aggregate(x, by, mean)
   X
   isQuarterly(X)
   
## Aggregate daily records to end of month records -
   X <- daily2monthly(x)
   X
   isMonthly(X)
   
## Aggregate da, ily records to end of week records -
   X <- daily2weekly(x, startOn="Fri")
   X
   dayOfWeek(time(X))
   
}


\keyword{methods}
\keyword{chron}

