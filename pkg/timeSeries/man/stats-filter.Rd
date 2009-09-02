
\name{filter}


\title{Linear Filtering on a Time Series}


\alias{filter,timeSeries-method}


\description{

     Applies linear filtering to a univariate time series or to each
     series separately of a multivariate time series.
      
}


\value{

  A timeSeries object without missing values.
  
}


\examples{
## Dummy timeSeries
   data <- matrix(rnorm(24), ncol = 2)
   s <- timeSeries(data, timeCalendar())
   
## Filter
   filter(s, rep(1, 3))
}
