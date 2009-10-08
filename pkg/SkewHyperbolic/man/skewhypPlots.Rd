\name{SkewHyperbolicPlots}
\alias{qqskewhyp}
\alias{ppskewhyp}
\title{
  Skew Hyperbolic Student t-Distribution Quantile-Quantile and
  Percent-Percent Plots
}
\description{
  \code{qqskewhyp} produces a skew hyperbolic \emph{t}-distribution Q-Q
  plot of the values in \code{y}, \code{ppskewhyp} produces a skew
  hyperbolic \emph{t}-distribution P-P (percent-percent) plot or
  probability plot of the values in \code{y}.  Graphical parameters may
  be given as arguments to \code{qqskewhyp} and \code{ppskewhyp}.
}
\usage{
qqskewhyp(y, mu = 0, delta = 1, beta = 1, nu = 1,
          param = c(mu, delta, beta, nu),
          main = "Skew Hyperbolic Student-t QQ Plot",
          xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
          plot.it = TRUE, line = TRUE, ...)
ppskewhyp(y, beta = NULL, delta = NULL, mu = NULL, nu = NULL,
          param = c(mu, delta, beta, nu),
          main = "Skew Hyperbolic Student-t P-P Plot",
          xlab = "Uniform Quantiles",
          ylab = "Probability-integral-transformed Data",
          plot.it = TRUE, line = TRUE, ...)
}
\arguments{
  \item{y}{The sample data.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Location parameter \eqn{\delta}{delta}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
  \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,delta,beta,nu)}.}
  \item{main,xlab,ylab}{Plot labels.}
  \item{plot.it}{Logical; if \code{plot.it = TRUE} the results will be plotted.}
  \item{line}{Logical; if \code{line = TRUE} a line is added through the
    origin with unit slope.}
  \item{\dots}{Further graphical parameters.}
}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones.
  }
\value{
  For \code{qqskewhyp} and \code{ppskewhyp}, a list with components:
  \item{x}{The x coordinates of the points to be plotted.}
  \item{y}{The y coordinates of the points to be plotted.}
}
\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's \emph{t}-distribution,
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.
}
\author{
David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\seealso{
\code{\link{ppoints}}, \code{\link{qqplot}}, \code{\link{dskewhyp}}
}
\examples{
par(mfrow = c(1,2))
param <- c(0,1,0,10)
y <- rskewhyp(500, param = param)
qqskewhyp(y, param = param, main = "Skew Hyperbolic\n Q-Q Plot")
ppskewhyp(y, param = param, main = "Skew Hyperbolic\n P-P Plot")
}
\keyword{distribution}
\keyword{hplot}
