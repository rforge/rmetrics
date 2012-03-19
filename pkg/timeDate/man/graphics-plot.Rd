\name{plot-methods}

\title{Plot Methods}


\alias{plot,timeDate-method}
\alias{points,timeDate-method}
\alias{lines,timeDate-method}
\alias{abline,ANY,ANY,ANY,timeDate-method}
\alias{axis.timeDate}
\alias{pretty.timeDate}


\description{
    Plot methods for \code{timeDate} objects.
}


\usage{
\S4method{plot}{timeDate}(x, y, \dots)
\S4method{lines}{timeDate}(x, y, \dots)
\S4method{points}{timeDate}(x, y, \dots)

axis.timeDate(side, x, at, format = NULL, labels = TRUE, \dots)

\method{pretty}{timeDate}(x, n=5, min.n=n\%/\%3, shrink.sml=0.75,
        high.u.bias=1.5, u5.bias=0.5+1.5*high.u.bias,
        eps.correct=0, \dots)
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
        a POSIX format string, e.g. "\%Y-\%m-\%d".
    }
    \item{labels}{
        either a logical value specifying whether annotations
        are to be made at the tickmarks, or a vector of character
        strings to be placed at the tickpoints.
    }
    \item{n}{
        an integer giving the desired number of intervals.
    }
    \item{min.n}{
        a nonnegative integer giving the minimal
        number of intervals.
    }
    \item{shrink.sml}{
        a positive numeric by a which a default
        scale is shrunk in the case when range(x) is
        very small.
    }
    \item{high.u.bias}{
        a non-negative numeric, typically > 1.
        Larger high.u.bias values favor larger units.
    }
    \item{u5.bias}{
        a non-negative numeric multiplier favoring
        factor 5 over 2.
    }
    \item{eps.correct}{
       an integer code, one of {0,1,2}. If
       non-0, a correction is made at the boundaries.
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

