\name{MillsRatio}
\alias{MillsRatio}
\alias{millsR}


\title{Mills Ratio}
\description{
  Calculates the Mills ratio
}
\usage{
millsR(y, log = FALSE)

}
\arguments{
  \item{y}{Numeric. Value at which the Mills' Ratio is evaluated.}
  \item{log}{Logical. If \code{log = TRUE}, Mills' Ratios are given as
   log(millsR).}

}

\value{
  The Mills' Ratio is
  \deqn{R(z)=\frac{1-\Phi(z)}{\phi(z)}}{R(z)=(1-Phi(z))/phi(z)}%
  where \eqn{\Phi(z)}{Phi(z)} and \eqn{\phi(z)}{phi(z)} are
  respectively the distribution function and density function of the
  standard normal distribution.
}

\details{
  The function calculates the Mills' Ratio.
  Since the Mill's Ratio converges to zero for large positive \eqn{z}{z}
  and infinity for large negative \eqn{z}{z}. The range over which the
  logarithm of the Mill's ratio may be calculated is greater than that
  for which the Mill's ratio itself may be calculated.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Jason Shicong Fu}



\examples{

## compare millsR calculated directly with the millsR calculated
## by transforming to log scale and then back-transformed
millsR(1:10)
exp(millsR(1:10, log = TRUE))
exp(millsR(10*(1:10)))
exp(millsR(10*(1:10), log = TRUE))
}

\keyword{math}
