\name{t}


\title{timeSeries Transpose}


\alias{t,timeSeries-method}


\description{
     Returns the transpose of a timeSries object.
}


\usage{
\S4method{t}{timeSeries}(x)
}


\arguments{
  \item{x}{
    a timeSeries object.
  }
}


\value{

  A matrix object.
}


\examples{
## Dummy timeSeries with NAs entries
   data <- matrix(1:24, ncol = 2)
   s <- timeSeries(data, timeCalendar())
   
## transpose
   t(s)
}
