\name{plot-methods}
\title{Plot Methods}

\alias{plot.timeDate}
\alias{points.timeDate}
\alias{lines.timeDate}
\alias{axis.timeDate}


\description{
    Plot methods for \code{timeDate} objects.
}

\usage{
\method{plot}{timeDate}(x, y, \dots)
\method{lines}{timeDate}(x, y, \dots)
\method{points}{timeDate}(x, y, \dots)
\method{axis}{timeDate}(side, x, at, format = NULL, labels = TRUE, \dots)
}
\arguments{
  \item{x, y, at}{
    an object of class \code{timeDate}.
  }
  \item{side}{
    an integer specifying which side of the plot the axis
    is to be drawn on. The axis is placed as follows:
    1=below, 2=left, 3=above and 4=right.
  }
  \item{format}{
    format - format string.
  }
  \item{labels}{
    either a logical value specifying whether annotations
    are to be made at the tickmarks, or a vector of character
    strings to be placed at the tickpoints.
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
    These S3 methods will become S4 methods in the future.
}

\examples{
## timeCalendar -
   x <- timeCalendar()
   y <- rnorm(12)

## Plotting :

   plot(x, y, type = "l")
   points(x, y, pch = 19, col = "red")

   plot(x, y, type = "l", xaxt = "n")
   axis.timeDate(1, at = x[c(1, 3, 5, 7, 9, 11)], format = "\%b")
   axis.timeDate(1, at = x[12], format = "\%Y")
}
\keyword{hplot}
\keyword{chron}
