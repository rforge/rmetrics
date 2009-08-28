\name{series-methods}
\docType{methods}

\alias{series}
\alias{series<-}
\alias{series,timeSeries-method}
\alias{series<-,timeSeries,matrix-method}
\alias{series<-,timeSeries,ANY-method}
\alias{series<-,timeSeries,data.frame-method}
\alias{series<-,timeSeries,vector-method}

\title{Data of a timeSeries object}


\description{
\code{series} returns \code{Data} slot a \code{timeSeries} object in a
\code{matrix} form. New series can also be assign to the \code{timeSeries}.
}

\usage{
%\S4method{series}{timeSeries}(x)
series(x)
series(x) <- value
}


\arguments{
  \item{x}{
    a \code{timeSeries} object.
  }
  \item{value}{
    a \code{vector}, a \code{data.frame} or a \code{matrix} object of
  numeric data. 
  }
}

\seealso{timeSeries()}

\examples{
## A Dummy timeSeries Object
   ts <- timeSeries()
   ts

## Get the Matrix Part - 
   mat <- series(ts)
   class(mat)
   mat

## Assign a New Univariate Series - 
   series(ts) <- rnorm(12)
   ts
   
## Assign a New Bivariate Series - 
   series(ts) <- rnorm(12)
   ts
}


