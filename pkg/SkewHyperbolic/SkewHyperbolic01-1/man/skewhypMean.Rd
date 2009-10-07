\name{skewhypMean}
\alias{skewhypMean}
\alias{skewhypVar}
\alias{skewhypSkew}
\alias{skewhypKurt}
\alias{skewhypMode}
\title{
Moments and Mode of the Skewed Hyperbolic Student's t-Distribution.
}
\description{
  Functions to calculate the mean, variance, skewness and kurtosis of a
  specified skewed hyperbolic t-distribution.

}
\usage{
skewhypMean(mu = 0, delta = 1, beta = 1, nu = 1, param = c(mu, delta,
  beta, nu))
skewhypVar(mu = 0, delta = 1, beta = 1, nu = 1, param = c(mu, delta,
  beta, nu))
skewhypSkew(mu = 0, delta = 1, beta = 1, nu = 1, param = c(mu, delta,
  beta, nu))
skewhypKurt(mu = 0, delta = 1, beta = 1, nu = 1, param = c(mu, delta,
  beta, nu))
skewhypMode(mu = 0, delta = 1, beta = 1, nu = 1, param = c(mu, delta,
  beta, nu), tolerance = .Machine$double.eps ^ 0.5)
}
\arguments{
\item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
\item{delta}{Peakedness parameter \eqn{\sigma}{sigma}, default is 1.}
\item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1. Negative
  values give a negative skew, positive values give a positive skew.}
\item{nu}{Shape parameter \eqn{\nu}{\nu}, default is 1.}
\item{param}{Specifying the parameters as a vector of the form
  \code{c(mu, delta, beta, nu)}.}
\item{tolerance}{A difference smaller than this value is taken to be zero.}
}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones.

  The moments are calculated as per formulae in Aas&Haff(2006) and the
  mode is calculated by numerical optimisation of the density function
  using \code{\link{optim}}.

  Note that the mean does not exist when \eqn{\nu = 2}{nu = 2}, the
  variance does not exist for \eqn{\nu \le 4}{nu <= 4}, the skewness does
  not exist for \eqn{\nu \le 6}{nu <= 6}, and the kurtosis does not exist
  for \eqn{\nu \le 8}{nu <= 8}.
  }
\value{
 \code{skewhypMean} gives the mean of the skewed hyperbolic
  t-distribution, \code{skewhypVar} the variance, \code{skewhypSkew} the
  skewness, \code{skewhypKurt} the kurtosis and \code{skewhypMode} the
  mode.
}

\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's t-Distribution.
  In \emph{Jorunal of Financial Econometrics}, 26-Jan-2006
  %%vol? pages?%%
}
\author{
David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\seealso{
\code{\link{dskewhyp}}, \code{\link{optim}}
}
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
}
\keyword{distribution}

