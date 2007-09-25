\name{financial}


\title{Financial Time Series}


\alias{financial}

\alias{returnSeries}
\alias{getReturns}
\alias{cumulatedSeries}
\alias{durationSeries}
\alias{midquoteSeries}
\alias{spreadSeries}


\description{
    
    A collection and description of functions 
    and methods dealing with special financial 
    timeseries objects.  
    \cr       

    \tabular{ll}{   
    \code{returnSeries} \tab Computes returns from a 'timeSeries' object, \cr
    \code{getReturns} \tab Computes returns from a 'timeSeries' object, \cr
    \code{cumulatedSeries} \tab Computes cumulated returns from a 'timeSeries', \cr
    \code{durationSeries} \tab Computes durations from a 'timeSeries' object, \cr
    \code{midquoteSeries} \tab Computes mid quotes from a 'timeSeries' object, \cr
    \code{spreadSeries} \tab Computes spreads from a 'timeSeries' object. }
       
}


\usage{
returnSeries(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 8, units = NULL)  
cumulatedSeries(x, index = 100)
durationSeries(x, trim = FALSE, units = c("secs", "mins", "hours"))   
midquoteSeries(x, which = c("Bid", "Ask"))
spreadSeries(x, which = c("Bid", "Ask"), tickSize = NULL)

getReturns(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 8, units = NULL)
}


\arguments{

    \item{digits}{
        [returnSeries] - \cr
        an integer value. The number of digits to be printed in the 
        output.
        }
    \item{index}{
        [cumulatedSeries] - \cr
        a numeric value to which the cumulated return series will be indexed.
        }
    \item{percentage}{
        [returnSeries] - \cr
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
        }
    \item{tickSize}{
        [spreadSeries] - \cr
        the default is NULL to simply compute price changes in original 
        price levels. If ticksize is supplied, the price changes will be 
        divided by the value of \code{inTicksOfSize} to compute
        price changes in ticks.
        }    
    \item{trim}{
        [returnSeries] - \cr
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{type}{
        [returnSeries] - \cr
        a character string specifying how to compute the returns. Valid 
        choices are: \code{continuous} and \code{discrete}. For the 
        default \code{type="continuous"}, the returns are calculated as 
        the logarithmic differences, otherwise if \code{type="discrete"}, 
        the returns are calculated as percentage changes. 
        }
    \item{units}{
        [returnSeries] - \cr
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        \cr
        [durationSeries] - \cr
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        }     
    \item{which}{
        [midquoteSeries][spreadSeries] - \cr
        a vector with two character strings naming the column names of
        the time series from which to compute the mid quotes and spreads.
        By default these are bid and ask prices with column names
        \code{c("Bid", "Ask")}.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        }
        
}


\value{

    all functions return an object of class \code{timeSeries}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))[1:10, 1:4]
   head(MSFT)

## returnSeries -  
   # Continuous Returns:
   returnSeries(MSFT)
   # Discrete Returns:
   returnSeries(MSFT, type = "discrete")
   # Don't trim:
   returnSeries(MSFT, trim = FALSE)
   # Use Percentage Values:
   returnSeries(MSFT, percentage = TRUE, trim = FALSE)
}


\keyword{chron}

