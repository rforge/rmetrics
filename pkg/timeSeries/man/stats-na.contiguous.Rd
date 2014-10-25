\name{na.contiguous}


\title{Find Longest Contiguous Stretch of non-NAs}


\alias{na.contiguous,timeSeries-method}


\description{

     Find the longest consecutive stretch of non-missing values in a
     timeSeries object.  (In the event of a tie, the first such
     stretch.)
      
}


\usage{
\S4method{na.contiguous}{timeSeries}(object, ...)
}


\arguments{
  \item{object}{
    a timeSeries object.
  }
  \item{\dots}{
    further arguments passed to or from other methods.
  }  
}


\value{

  A timeSeries object without missing values.
  
}


\examples{
## Dummy timeSeries with NAs entries
   data <- matrix(sample(c(1:20, rep(NA,4))), ncol = 2)
   s <- timeSeries(data, timeCalendar())
   
## Find the longest consecutive non-missing values
   na.contiguous(s)
}
