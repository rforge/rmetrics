\name{drawdowns}


\title{Calculations of Drawdowns}


\alias{drawdowns}
\alias{drawdownsStats}


\description{
    
    Compute series of drawdowns from financial returns 
    and calculate drawdown statisitcs.      
    
}
    
    
\usage{
drawdowns(x, \dots)

drawdownsStats(x, \dots)
}


\arguments{

    \item{x}{
        a 'timeSeries' object of financial returns.  
        Note, drawdowns can be calculated from an uni- or multivariate 
        time deries object, statistics can only be computed from an
        univariate time series object.
        }
    \item{\dots}{
        optional arguments passed to the function \code{na.omit}.
        }
        
}


\value{

    \code{drawdowns}\cr
    returns an object of class 'timeSeries'.
    
    \code{drawdownsStats}\cr
    returns an object of class 'data.frame' with the following entries:\cr 
    \code{"drawdown"} - the depth of the drawdown, \cr 
    \code{"from"} - the start date, \cr  
    \code{"trough"} - the trough period, \cr 
    \code{"to"} - the end date, \cr 
    \code{"length"} - the length in number of records, \cr 
    \code{"peaktrough"} - the peak trough, and , \cr 
    \code{"recovery"} - the recovery length in number of records.
        
}

    
\details{

    The code in the core of the function \code{drawdownsStats} was 
    was borrowed from the package \code{PerformanceAnalytics} 
    authored by Peter Carl and Sankalp Upadhyay.
    
}

   
\author{

    Peter Carl and Sankalp Upadhyay for code from the contributed 
    R package \code{PerformanceAnalytics} used in the function 
    \code{drawdownsStats}.
    
}


\examples{
## Use Swiss Pension Fund Data Set of Returns - 
   head(LPP2005REC)
   SPI <- LPP2005REC[, "SPI"]
   head(SPI)
   
## Plot Drawdowns - 
   dd = drawdowns(LPP2005REC[, "SPI"], main = "Drawdowns")
   plot(dd)
   dd = drawdowns(LPP2005REC[, 1:6], main = "Drawdowns")
   plot(dd)

## Compute Drawdowns Statistics - 
   ddStats <- drawdownsStats(SPI)
   class(ddStats)
   ddStats
   
## Note, Only Univariate Series are allowd -
   ddStats <- try(drawdownsStats(LPP2005REC))
   class(ddStats)
}


\keyword{chron}

