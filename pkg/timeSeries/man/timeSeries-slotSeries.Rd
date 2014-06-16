\name{series-methods}


\docType{methods}


\alias{series}
\alias{series<-}

\alias{series,timeSeries-method}
\alias{series<-,timeSeries,matrix-method}
\alias{series<-,timeSeries,ANY-method}
\alias{series<-,timeSeries,data.frame-method}
\alias{series<-,timeSeries,vector-method}

\alias{coredata}
\alias{coredata<-}

\alias{coredata,timeSeries-method}
\alias{coredata<-,timeSeries,matrix-method}
\alias{coredata<-,timeSeries,ANY-method}
\alias{coredata<-,timeSeries,data.frame-method}
\alias{coredata<-,timeSeries,vector-method}

%\alias{getSeries}
%\alias{getSeries.timeSeries}
%\alias{setSeries<-}


\title{Get and Set Data of a 'timeSeries'}


\description{

    \code{series} returns the \code{@.Data} slot of a \code{timeSeries} 
    object in \code{matrix} form. New series can also be assign to an
    already existing \code{timeSeries}.
    
    \code{coredata} is a synonyme function nameing for \code{series}.
    
}


\usage{
%\S4method{series}{timeSeries}(x)
series(x)
series(x) <- value
%
%getSeries(x)
%setSeries(x) <- value
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


\keyword{programming}

