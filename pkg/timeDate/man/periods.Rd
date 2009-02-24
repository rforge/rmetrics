\name{periods}


\title{Rolling periods}


\alias{periods}
\alias{periodicallyRolling}
\alias{monthlyRolling}



\description{
  Returns start and end dates for a rolling periods
}

  
\usage{
periods(x, period = "12m", by = "1m", offset = "0d")
periodicallyRolling(x, period = "52w", by = "4w", offset = "0d")
monthlyRolling(x, period = "12m", by = "1m")
}

  
\arguments{
  \item{x}{an object of class timeDate}
  \item{period}{a span string, consisting of a length integer
    and a unit value, e.g. "52w" for 52 weeks.}
  \item{by}{a span string, consisting of a length integer
    and a unit value, e.g. "4w" for 4 weeks.}
  \item{offset}{a span string, consisting of a length integer
    and a unit value, e.g. "0d" for no offset.}
}

\details{

  Periodically Rolling - Allowed unit values are "m" for 4 weeks, "w"
  for weeks, "d" for days, "H" for hours, "M" for minutes, and "S" for
  seconds.


  Monthly Calendar Rolling - The only allowed allowed unit value is "m"
  for monthly periods. Express a quarterly period by "3m", a semester by
  "6m", a year by "12m" etc.

}

\examples{
x <- timeSequence(from = "2001-01-01", to = "2009-01-01", by = "day") 

periods(x, "12m", "1m")
periods(x, "52w", "4w")
    
periodicallyRolling(x)
    
monthlyRolling(x)
}
