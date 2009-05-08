\name{ghyp.moment}
\alias{ghyp.moment}
\title{Compute moments of generalized hyperbolic distributions}
\description{

  This function computes moments of arbitrary orders of the univariate
  generalized hyperbolic distribution. The expectation of \eqn{f(X -
  c)^k} is calculated. \eqn{f} can be either the absolute value or the
  identity. \eqn{c} can be either zero or \eqn{E(X)}.

}
% <---------------------------------------------------------------------->
\usage{
ghyp.moment(object, order = 3:4, absolute = FALSE, central = TRUE, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{A univarite generalized hyperbolic object inheriting from class
               \code{\link[=ghyp-class]{ghyp}}.}
  \item{order}{A vector containing the order of the moments.}
  \item{absolute}{Indicate whether the absolute value is taken or
    not. If \code{absolute = TRUE} then \eqn{E(|X - c|^k)} is
    computed. Otherwise \eqn{E((X - c)^k)}. \eqn{c} depends on the
    argument \code{central}. \code{absolute} must be \code{TRUE}
    if \code{order} is not integer. }
  \item{central}{If \code{TRUE} the moment around the expected value
    \eqn{E((X - E(X))^k)} is computed. Otherwise \eqn{E(X^k)}.}
  \item{\dots}{Arguments passed to \code{\link{integrate}}.}
}
% <---------------------------------------------------------------------->
\details{

  In general \code{ghyp.moment} is based on numerical integration. For
  the special cases of either a \dQuote{ghyp}, \dQuote{hyp} or
  \dQuote{NIG} distribution analytic expressions (see \bold{References})
  will be taken if non-absolute and non-centered moments of integer
  order are requested.

}
% <---------------------------------------------------------------------->
\value{
  A vector containing the moments.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\references{
  \emph{Moments of the Generalized Hyperbolic Distribution} by
  David J. Scott, Diethelm Wuertz and Thanh Tam Tran \cr
  Working paper, 2008
}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=mean.ghyp]{mean}}, \code{\link[=vcov.ghyp]{vcov}},
  \code{\link{Egig}}}
% <---------------------------------------------------------------------->
\examples{
  nig.uv <- NIG(alpha.bar = 0.1, mu = 1.1, sigma = 3, gamma = -2)

  # Moments of integer order
  ghyp.moment(nig.uv, order = 1:6)

  # Moments of fractional order
  ghyp.moment(nig.uv, order = 0.2 * 1:20, absolute = TRUE)
}
% <---------------------------------------------------------------------->
\keyword{utilities}
