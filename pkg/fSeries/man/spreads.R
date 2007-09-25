\name{spreads}


\title{Calculations of spreads and mid quotes}


\alias{spreads}
\alias{midquotes}

\alias{spreadSeries}
\alias{midquoteSeries}


\description{
    
    Functions to calculate spreds and midquotes from price streams.
       
}


\usage{      
midquotes(x, which = c("Bid", "Ask"))
spreads(x, which = c("Bid", "Ask"), tickSize = NULL)

midquoteSeries(x, which = c("Bid", "Ask"))
spreadSeries(x, which = c("Bid", "Ask"), tickSize = NULL)
}


\arguments{

    \item{tickSize}{
        the default is NULL to simply compute price changes in original 
        price levels. If ticksize is supplied, the price changes will be 
        divided by the value of \code{inTicksOfSize} to compute
        price changes in ticks.
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

## midquotes -

## spreads -

}


\keyword{chron}

