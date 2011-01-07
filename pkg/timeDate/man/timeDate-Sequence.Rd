\name{timeSequence}

\alias{timeSequence}
\alias{seq.timeDate}

\title{Regularly spaced 'timeDate' objects}
\description{
    Create a regularly spaced object of class \code{timeDate}.
}

\usage{
timeSequence(from, to = Sys.timeDate(), by, length.out = NULL,
            format = NULL, zone = "", FinCenter = "")

\method{seq}{timeDate}(from, to, by, length.out = NULL, along.with = NULL, \dots)
}

\arguments{
  \item{from, to}{
    starting date, required, and end date, optional.  If supplied,
    \code{to} must be after (later than) \code{from}.
  }
  \item{by}{
    \itemize{
      \item a character string, containing one of "sec", "min",
      "hour", "day", "week", "month" or "year". This can
      optionally be preceded by an integer and a
      space, or followed by "s".
      \item character string "quarter" that is equivalent to "3 months".
      \item A number, taken to be in seconds.
      \item A object of class 'difftime'
    }
  }
  \item{length.out}{
    length.out integer, optional. Desired length
    of the sequence, if specified "to" will be ignored.
  }
  \item{along.with}{Take the length from the length of this argument.}
  \item{format}{the format specification of the input character vector.}
  \item{zone}{the time zone or financial center where the data were recorded.}
  \item{FinCenter}{
    a character with the the location of the
    financial center named as "continent/city".
  }
  \item{\dots}{arguments passed to other methods.}
}

\note{
  \code{timeSequence()} is a wrapper for the \code{"timeDate"} method of
  \code{\link{seq}()}, and that has been closely modeled after base \R's
  \code{POSIXt} method, \code{\link{seq.POSIXt}}.
}

\value{
  an object of \code{\link{class}} \code{"\linkS4class{timeDate}"}.
}

\examples{
## timeSequence -

## autodetection of format :
(t1 <- timeSequence(from = "2004-03-12", to = "2004-04-11"))

stopifnot( ## different formats even:
  identical(t1, timeSequence(from = "2004-03-12", to = "11-Apr-2004")),
  identical(t1, ## explicit format and FinCenter :
      timeSequence(from = "2004-03-12", to = "2004-04-11",
                   format = "\%Y-\%m-\%d", FinCenter = "GMT")))

## observe "switch to summer time":
timeSequence(from = "2004-03-12", to = "2004-04-11",
             FinCenter = "Europe/Zurich")
}

\keyword{chron}

