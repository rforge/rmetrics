\name{summary-methods}


\title{Summary Method}


\alias{summary.timeDate}


\description{

    Summarizes details of a 'timeDate' object.
}


\usage{
\method{summary}{timeDate}(object, \dots)
}


\arguments{

    \item{object}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    returns a summary report of the details of a \code{timeDate}
    object. This includes the starting and end date, the number of
    dates the format and the financial center in use.
   
}


\note{

    This S3 method will become in the future a S4 method
    
}


\examples{
## summary -
   tC = timeCalendar()
   summary(tC)
}


\keyword{chron}

