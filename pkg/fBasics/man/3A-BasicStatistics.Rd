\name{BasicStatistics}

\alias{BasicStatistics}

\alias{basicStats}

\alias{stdev}

\title{Basic Statistics Summary}


\description{

    A collection and description of functions to compute 
    basic statistical properties of financial and
    economic time series data.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{basicStats} \tab computes an overview of basic statistical values. }
    
    For SPLUS Compatibility:
    
    \tabular{ll}{
    \code{stdev} \tab Returns the standard deviation of a vector or matrix. }

}


\usage{
stdev(x, na.rm = FALSE)

basicStats(x, ci = 0.95)
}


\arguments{
  
    \item{ci}{
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a numeric vector.
        \cr
        [basicStats] - \cr
        allows also a matrix, data.frame or timeSeries object as input.
        In this case only the first column of data will be considered
        and a a warning will be printed.        
        }
        
}


\value{
    
    \code{basicsStats}
    \cr
    returns data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    
}


\note{

    The function \code{stdev} computes the standard deviation for a
    vector or matrix and was introduced for SPlus compatibility.
    Under R use the function \code{sd}. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.3A-BasicStatistics")

## basicStats -
   # Simulated Monthly Return Data:
   tS = timeSeries(matrix(rnorm(12)), timeCalendar())
   # ... must be univariate:
   basicStats(tS)  
}


\keyword{univar}

