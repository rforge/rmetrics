% --- Source file: qqvg.Rd ---
\name{VarianceGammaPlots}
\alias{qqvg}
\alias{ppvg}

\title{Variance Gamma Quantile-Quantile and Percent-Percent Plots}

\description{
  \code{qqvg} produces a variance gamma Q-Q plot of the values in
  \code{y}.

  \code{ppvg} produces a variance gamma P-P (percent-percent) or
  probability plot of the values in \code{y}.
  Graphical parameters may be given as arguments to \code{qqvg} and
  \code{ppvg}.
}

\usage{

  qqvg(y, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC, sigma, theta, nu), main = "Variance Gamma Q-Q Plot",
    xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
    plot.it = TRUE, line = TRUE, \dots)

  ppvg(y, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC, sigma, theta, nu), main = "Variance Gamma P-P Plot",
    xlab = "Uniform Quantiles",
    ylab = "Probability-integral-transformed Data", plot.it = TRUE,
    line = TRUE, \dots)

}

\arguments{
  \item{y}{The data sample.}
  \item{vgC}{The location parameter \eqn{c}{c}, default is 0.}
  \item{sigma}{The spread parameter \eqn{\sigma}{sigma}, default is 1,
    must be positive.}
  \item{theta}{The asymmetry parameter \eqn{\theta}{theta}, default is 0.}
  \item{nu}{The shape parameter \eqn{\nu}{nu}, default is 1, must be
    positive.}
  \item{param}{An optional option, specifying the parameters as a vector
    which takes the form \code{c(vgC,sigma,theta,nu)} if known.}
  \item{main}{Plot title.}
  \item{xlab, ylab}{Plot labels.}
  \item{plot.it}{Logical. Should the result be plotted?}
  \item{line}{Add line through origin with unit slope.}
  \item{\dots}{Further graphical parameters.}
}

\details{
  Users may specify the parameter values of the data sample \code{y} using
  argument \code{param}. If \code{param} is not specified by users, then
  the values are estimated from \code{y} by \code{vgFit}. For more details of
  fiting a variance gamma distribution to data, see \code{\link{vgFit}}.

}
\value{
  For \code{qqvg} and \code{ppvg}, a list with components:
  \item{x}{The x coordinates of the points that are to be plotted.}
  \item{y}{The y coordinates of the points that are to be plotted.}
}
\references{
  Wilk, M. B. and Gnanadesikan, R. (1968)
  Probability plotting methods for the analysis of data.
  \emph{Biometrika}.
  \bold{55}, 1--17.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\seealso{
  \code{\link{ppoints}}, \code{\link{dvg}}.
}


\examples{

## Example 1: the parameter values are known
par(mfrow = c(1,2))
y <- rvg(200, param = c(2,2,1,2))
qqvg(y, param = c(2,2,1,2),line = FALSE)
abline(0, 1, col = 2)
ppvg(y, param = c(2,2,1,2))

## Example 2: the parameter values are unknown
par(mfrow = c(1,2))
y <- rvg(200, param = c(2,2,1,2))
qqvg(y, line = FALSE)
abline(0, 1, col = 2)
ppvg(y)
}