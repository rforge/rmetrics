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
  Since the Mill's Ratio is not stable for \eqn{z}{z} greater than about
  7, we use the log function to get around the instability.


}

\author{David Scott \email{d.scott@auckland.ac.nz}, Jason Shicong Fu}



\examples{

## compare millsR calculated directly with the millsR calculated
## by transforming to log scale and then back-transformed
unlist(lapply(1:10, millsR))
exp(unlist(lapply(1:10, millsR, log = TRUE)))
exp(unlist(lapply(10*(1:10), millsR)))
exp(unlist(lapply(10*(1:10), millsR, log = TRUE)))


}

\keyword{math}
