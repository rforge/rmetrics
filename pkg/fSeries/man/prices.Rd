\name{prices}


\title{Calculations from Price Streams}


\alias{prices}

\alias{returns}
\alias{midquotes}
\alias{spreads}

\alias{returnSeries}
\alias{getReturns}
\alias{midquoteSeries}
\alias{spreadSeries}


\description{
    
    Functions to calculate derivated timeSeries from price
    streams like returns, mid quotes, and spreads.
       
}


\usage{
returns(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 8, units = NULL)  
midquotes(x, which = c("Bid", "Ask"))
spreads(x, which = c("Bid", "Ask"), tickSize = NULL)

returnSeries(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 8, units = NULL)  
getReturns(x, type = c("continuous", "discrete"), percentage = FALSE, 
    trim = TRUE, digits = 8, units = NULL)
midquoteSeries(x, which = c("Bid", "Ask"))
spreadSeries(x, which = c("Bid", "Ask"), tickSize = NULL)
}


\arguments{

    \item{digits}{
        an integer value. The number of digits to be printed in the 
        output.
        }
    \item{percentage}{
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
        }
    \item{tickSize}{
        the default is NULL to simply compute price changes in original 
        price levels. If ticksize is supplied, the price changes will be 
        divided by the value of \code{inTicksOfSize} to compute
        price changes in ticks.
        }    
    \item{trim}{
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{type}{
        a character string specifying how to compute the returns. Valid 
        choices are: \code{continuous} and \code{discrete}. For the 
        default \code{type="continuous"}, the returns are calculated as 
        the logarithmic differences, otherwise if \code{type="discrete"}, 
        the returns are calculated as percentage changes. 
        }
    \item{units}{
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        }     
    \item{which}{
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


\note{

    The functions \code{returnSeries}, \code{getReturns}, 
    \code{midquoteSeries}, \code{spreadSeries} are synonymes
    for \code{returns}, \code{midquotes}, and \code{spreads}.

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
   returns(MSFT)
   # Discrete Returns:
   returns(MSFT, type = "discrete")
   # Don't trim:
   returns(MSFT, trim = FALSE)
   # Use Percentage Values:
   returns(MSFT, percentage = TRUE, trim = FALSE)
}


\keyword{chron}

