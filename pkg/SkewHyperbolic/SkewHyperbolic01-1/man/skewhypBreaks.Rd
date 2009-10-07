\name{skewhypBreaks}
\alias{skewhypBreaks}
\alias{skewhypCalcRange}
\alias{ddskewhyp}
\title{
Break points for the Skewed Hyperbolic Student's t-Distribuiton
}
\description{
  Utility routines that calculate suitable breakpoints for use in
  determining the distribution function, and the derivative of the
  density function.
}
\usage{
skewhypBreaks(mu = 0, delta = 1, beta = 1, nu = 1, param =
  c(mu, delta, beta, nu), small = 10^(-6), tiny = 10^(-10),
  deriv = 0.3, ...)
skewhypCalcRange(mu = 0, delta = 1, beta = 1, nu = 1,
  param = c(mu,delta,beta,nu), tol= 10^(-5), ...)
ddskewhyp(x, mu = 0, delta = 1, beta = 1, nu = 1, param =
  c(mu,delta,beta,nu),log = FALSE, tolerance =
  .Machine$double.eps ^ 0.5)
}
\arguments{
 \item{x}{Vector of quantiles}
 \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
 \item{delta}{Peakedness parameter \eqn{\delta}{delta}, default is 1.}
 \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
 \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
 \item{param}{Specifying the parameters as a vector of the form
   \code{c(mu, delta, beta, nu)}.}
 \item{small}{Size of a small difference between the distribution
   function and 0 or 1.}
 \item{tiny}{Size of a tiny difference between the distribution
   function and 0 or 1.}
 \item{deriv}{Value between 0 and 1 which determines the point at which
   the value of the derivative becomes substantial compared to its
   maximal value, see \bold{Details}.}
 \item{log}{Logical; if \code{log = TRUE}, probabilities are given as log(p).}
 \item{tolerance}{Specified level of tolerance when checking if parameter beta
   is equal to 0 in \code{ddskewhyp}.}
 \item{tol}{Tolerance used in \code{skewhypCalcRange}, see
   \bold{Details}.}
 \item{\dots}{Passes additional arguments to \code{\link{uniroot}}.}
}
\value{
  \code{ddskewhyp} gives the derivative of \code{\link{dskewhyp}}.

  \code{skewhypBreaks} returns a list with components:
  \item{xTiny}{Value such that the probabilities to the left are less
    than \code{tiny}.}
  \item{xSmall}{Value such that probabilities to the left are less
    than \code{small}.}
  \item{lowBreak}{Point to the left of the mode such that the
    derivative of the density at that point is \code{deriv} times its
    maximum value on that side of the mode.}
  \item{highBreak}{Point to the right of the mode such that the derivative
    of the density at that point is \code{deriv} times the value of the
    maximum value on that side of the mode.}
  \item{xLarge}{Value such that the probabities to the right are less than
    \code{xSmall}.}
  \item{xHuge}{Value such that probability to the right is less than
    \code{tiny}.}
  \item{modeDist}{The mode of the given skewed hyperbolic distribution,
    calculated by \code{\link{skewhypMode}}.}

  \code{skewhypCalcRange} returns the quantile values at the
    endpoints of the region of the density function where the probablity
    is greater than \code{tol}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}

\seealso{
\code{\link{uniroot}}, \code{\link{dskewhyp}}, \code{\link{skewhypMode}}
}
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

