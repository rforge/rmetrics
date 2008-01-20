\name{BasicStatistics}


\alias{basicStats}


\title{Basic Time Series Statistics}


\description{
    
    Computes basic financial time series statististics.
    \cr

    List of Functions:
    
    \tabular{ll}{
    \code{basicStats} \tab Computes an overview of basic statistical values.}
    
}
    

\usage{ 
basicStats(x, ci = 0.95)
}


\arguments{
  
    \item{ci}{
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{x}{
        an object of class \code{"timeSeries"} or any other object which
        can be transformed by the function \code{as.timeSeries} into an
        object of class \code{timeSeries}. The latter case, other then
        \code{timeSeries} objects, is more or less untested.
        }  
}


\value{
       
    \code{basicsStats}
    \cr
    returns a data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## basicStats -
   # Simulated Monthly Return Data:
   tS = timeSeries(matrix(rnorm(12)), timeCalendar())
   basicStats(tS)
}


\keyword{programming}

