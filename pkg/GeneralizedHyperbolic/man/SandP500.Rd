\name{SandP500}
\docType{data}
\alias{SandP500}
\title{S\&P 500}
\description{
  This data set gives the value of Standard and Poor's most notable stock
  market price index (the S\&P 500) at year end, from 1800 to 2001.
}
\usage{data(SandP500)}
\format{
  A vector of 202 observations.
}
\source{
  \url{http://www.globalfindata.com}
}
\references{
  Brown, Barry W., Spears, Floyd M. and Levy, Lawrence B. (2002)
  The log \emph{F}: a distribution for all seasons.
  \emph{Computational Statistics},
  \bold{17}, 47--58.
}
\examples{
data(SandP500)
### Consider proportional changes in the index
change <- SandP500[-length(SandP500)] / SandP500[-1]
hist(change)
### Fit hyperbolic distribution to changes
hyperbFit(change)

}
\keyword{datasets}
