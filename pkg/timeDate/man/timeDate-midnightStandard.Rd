\name{midnightStandard}

\title{Midnight Standard}

\alias{midnightStandard}
\alias{midnightStandard2}

\description{
    Corrects 'timeDate' objects if they do not fulfill the
    ISO8601 midnight standard.

    \code{midnightStandard2()} relies on \code{\link{strptime}} wherever
    possible, and there simply returns
    \code{\link{as.POSIXct}(\link{strptime}(charvec, format, tz = "GMT"))}.
}
\usage{
midnightStandard (charvec, format)
midnightStandard2(charvec, format)
}
\arguments{
  \item{charvec}{a character string or vector of dates and times.}
  \item{format}{a string, the format specification of the input
    character vector.}
}

\value{
     \code{midnightStandard} returns a \code{character} and
     \code{midnightStandard2} a \code{\link{POSIXct}} object.
}


\examples{
   ch <- "2007-12-31 24:00"
   midnightStandard(ch)
   (ms2 <- midnightStandard2(ch))
   class(ms2)
}
\keyword{chron}
