\name{finCenter}


\alias{finCenter}
\alias{finCenter,timeDate-method}

\alias{finCenter<-}
\alias{finCenter<-,timeDate-method}

\title{Financial Center of a timeDate object}

\description{
  Print or assign new financial center to a \code{timeDate} object.
}

\usage{
\S4method{finCenter}{timeDate}(x)
\S4method{finCenter}{timeDate}(x) <- value
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
date <- timeDate("2008-01-01")
finCenter(date) <- "GMT"
date

finCenter(date) <- "Zurich"
date
}


