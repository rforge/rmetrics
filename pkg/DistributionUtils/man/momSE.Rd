\name{momSE}
\alias{momSE}

\title{
  Standard Errors of Sample Moments
}
\description{
  Calculates the approximate standard error of the sample variance,
  sample central third moment and sample central fourth moment.
}
\usage{
momSE(order = 4, n, mom)
}
\arguments{
  \item{order}{Integer: either 2, 3, or 4.}
  \item{n}{Integer: the sample size.}
  \item{mom}{Numeric: The central moments of order 1 to \eqn{2n} of the
  distribution being sampled from.}
}
\details{
  Implements the approximate standard error given in Kendall and Stuart
  (1969), p.243.
}
\value{
  The approximate standard error of the sample moment specified.
}
\references{
  Kendall, M. G. and Stuart, A. (1969).
  \emph{The Advanced Theory of Statistics, Volume 1, 3rd Edition}.
  London: Charles Griffin & Company.
}
  

\author{David Scott \email{d.scott@auckland.ac.nz}
}

\seealso{
\code{\link{momChangeAbout}}
}
\examples{
### Moments of the normal distribution, mean 1, variance 4
mu <- 1
sigma <- 2
mom <- c(0,sigma^2,0,3*sigma^4,0,15*sigma^6,0,105*sigma^8)
### standard error of sample variance
momSE(2, 100, mom[1:4])
### should be
sqrt(2*sigma^4)/10
### standard error of sample central third moment
momSE(3, 100, mom[1:6])
### should be
sqrt(6*sigma^6)/10
### standard error of sample central fourth moment
momSE(4, 100, mom)
### should be
sqrt(96*sigma^8)/10
}
\keyword{distribution}
\keyword{univar}