\name{.endpoints}


\alias{.endpoints}


\title{Endpoints Indexes}


\description{
Returns endpoint indexes from a 'timeDate' object
}


\usage{
.endpoints(x, on = c("months", "years", "quarters", "weeks", "days",
    "hours", "minutes", "seconds"), k=1)
}


\arguments{
    \item{x}{
        a \code{timeDate} object.
        }
    \item{on}{
        the periods endpoints to find as a character string.
        Select from: \code{"months"}, \code{"years"},
        \code{"quarters"}, \code{"weeks"}, \code{"days"},
        \code{"hours"}, \code{"minutes"}, \code{"seconds"}.
        }
    \item{k}{
        along every k-th element.
        }
}


\details{
    \code{endpoints} returns an integer vector corresponding to the last
    observation in each period specified by \code{on}, with a zero added
    to the beginning of the vector, and the index of the last observation
    in  \code{x} at the end.
}


\value{
    returns an intege vector of endpoints beginning with 0 and ending
    with the a value equal to the length of the \code{x} argument.
}


\author{
    Jeff Ryan, modofied by Diethelm Wuertz for \code{timeDate} objects.
}


\examples{
## endpoints -

   # Weekly Endpoints:
   .endpoints(timeCalendar(), on="w")
}


\keyword{programming}
