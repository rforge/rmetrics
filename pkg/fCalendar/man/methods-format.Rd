\name{plot-methods}


\title{Format Methods}


\alias{format}
\alias{format.timeDate}


\description{

    Formats 'timeDate' objects as ISO conform character strings.
}


\usage{
\method{format}{timeDate}(x, \dots)
}


\arguments{

    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    returns an ISO conform formatted character string.
   
}


\seealso{

    \code{as.character}.


\examples{
## timeCalendar -
   # Time Calebdar 16:00
   tC = timeCalendar() + 16*3600
   tC
   
## Format as ISO Character String: 
   format(tC)
}


\keyword{chron}

