\name{finCenter}

\alias{finCenter,timeSeries-method}
\alias{finCenter<-,timeSeries-method}

\title{Financial Center of to a timeSeries}

\description{
  Print or assign new financial center to a \code{timeSeries} object.
}

\usage{
\S4method{finCenter}{timeSeries}(x)
\S4method{finCenter}{timeSeries}(x) <- value

}


\arguments{
  \item{x}{
    a \code{timeSeries} object.
  }
  \item{value}{
    a character with the the location of the  financial center named 
    as "continent/city".
  }
}

\seealso{listFinCenter}

\examples{
# A timeSeries object
data <- matrix(rnorm(24), ncol = 2)
charvec <- as.character(timeCalendar())
ts <- timeSeries(data, charvec, FinCenter = "NewYork")
ts

# print financial center
finCenter(ts)

# assign new financial center
finCenter(ts) <- "Zurich"
ts
}


