\name{Bessel K Ratio}
\alias{besselRatio}


\title{Ratio of Bessel K Functions}
\description{
  Calculates the ratio of Bessel K functions of different orders
}
\usage{
besselRatio(x, nu, orderDiff, useExpScaled = 700)

}
\arguments{
  \item{x}{Numeric, \eqn{\geq 0}{>= 0}. Value at which the numerator and
  denominator Bessel functions are evaluated.}
  \item{nu}{Numeric. The order of the Bessel function in the
    denominator.}
  \item{orderDiff}{Numeric. The order of the numerator Bessel function
    minus the order of the denominator Bessel function.}
  \item{useExpScaled}{Numeric, \eqn{\geq 0}{>= 0}. The smallest value of
    \eqn{x}{x} for which the ratio is calculated using the
    exponentially-scaled Bessel function values.}
}

\value{
  The ratio
  \deqn{\frac{K_{\nu+k}(x)}{K_{\nu}(x)}}{K_(nu+k)(x)/K_nu(x)}
  of two modified Bessel functions of the third kind whose orders differ
  by \eqn{k}{k}.
}

\details{
  Uses the function \code{\link{besselK}} to calculate the ratio of two
  modified Bessel function of the third kind whose orders are
  different. The calculation of Bessel functions will underflow if the
  value of \eqn{x}{x} is greater than around 740. To avoid underflow the
  exponentially-scaled Bessel functions can be returned by
  \code{\link{besselK}}. The ratio is actually unaffected by exponential
  scaling since the scaling cancels across numerator and denominator.

  The Bessel function ratio is useful in calculating moments of the
  Generalized Inverse Gaussian distribution, and hence also for the
  moments of the hyperbolic and generalized hyperbolic distributions.
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{
  \code{\link{besselK}}, \code{\link[GeneralizedHyperbolic]{gigMom}}
}

\examples{
nus <- c(0:5, 10, 20)
x <- seq(1, 4, length.out = 11)
k <- 3

raw <- matrix(nrow = length(nus), ncol = length(x))
scaled <- matrix(nrow = length(nus), ncol = length(x))
compare <- matrix(nrow = length(nus), ncol = length(x))

for (i in 1:length(nus)){
    for (j in 1:length(x)) {
        raw[i,j] <- besselRatio(x[j], nus[i],
                                orderDiff = k)
        scaled[i,j] <- besselRatio(x[j], nus[i],
                                orderDiff = k, useExpScaled = 1)
        compare[i,j] <- raw[i,j]/scaled[i,j]
    }
}
raw
scaled
compare

}

\keyword{math}
