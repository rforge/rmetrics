\name{align}


\title{Making a 'timeDate' object unique}


\alias{align}
\alias{align,timeDate-method}
\alias{align,ANY-method}

\alias{alignDaily}
\alias{alignMonthly}
\alias{alignQuarterly}


\description{

  Aligns a 'timeDate' object to regular date/time stamps.

}


\usage{
\S4method{align}{timeDate}(x, by = "1d", offset = "0s")

alignDaily(x, include.weekends=FALSE)
alignMonthly(x, include.weekends=FALSE)
alignQuarterly(x, include.weekends=FALSE)
}


\arguments{
  \item{x}{
    an object of class \code{timeDate}.
  }
  \item{by}{
    a character string formed from an integer length and
    a period identifyer. Valid values are
    "w", "d", "h", "m", "s", for
    weeks, days, hours, minutes and seconds. For example
    a bi-weekly period is expressed as \code{"2w"}.
  }
  \item{offset}{
    a character string to set an offset formed from an integer
    length and a period identifyer in the same way as for the
    argument \code{by}.
  }
  \item{include.weekends}{
    logical value indicating whether 'weekends' should be
    included.
  }
}


\details{

  The functions \code{alignDaily}, \code{alignMonthly},
  \code{alignMonthly} are simple to use functions which
  generate end-of-day, end-of-month, and end-of quarter
  \code{timeDate} objects. Weekends are excluded by
  default. Optionally they can be added setting the
  argument \code{include.weekends=TRUE}.

}

\value{

  returns an object of class \code{timeDate}.

}


\examples{
## align -

# Align Bi-Weekly with a 3 Days Offset:
(tC <- timeCalendar())
align(tC, by = "2w", offset = "3d")

## alignDaily -

# Simple to use Functions:
alignDaily(tC)
alignDaily(tC, include.weekends=TRUE)

# Align to end-of-month Dates:
alignMonthly(tC)

}


\keyword{chron}

