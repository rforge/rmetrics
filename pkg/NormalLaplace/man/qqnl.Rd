\name{nlPlots}
\alias{qqnl}
\alias{ppnl}

\title{Normal Laplace Quantile-Quantile and Percent-Percent Plots}
\description{
  \code{qqnl} produces a normal Laplace Q-Q plot of the values in
	\code{y}.

  \code{ppnl} produces a normal Laplace P-P (percent-percent) or
  probability plot of the values in \code{y}.

  Graphical parameters may be given as arguments to \code{qqnl},
  and \code{ppnl}.
}

\usage{
qqnl(y, mu = 0, sigma = 1, alpha = 1, beta = 1,
     param = c(mu, sigma, alpha, beta),
     main = "Normal Laplace Q-Q Plot",
     xlab = "Theoretical Quantiles",
     ylab = "Sample Quantiles",
     plot.it = TRUE, line = TRUE, \dots)
ppnl(y, mu = 0, sigma = 1, alpha = 1, beta = 1,
     param = c(mu, sigma, alpha, beta),
     main = "Normal Laplace P-P Plot",
     xlab = "Uniform Quantiles",
     ylab = "Probability-integral-transformed Data",
     plot.it = TRUE, line = TRUE, \dots)
}

\arguments{
  \item{y}{The data sample.}
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{sigma}{\eqn{\sigma}{sigma} is the variance parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is a skewness parameter, with a default
    value of 1.}
  \item{beta}{\eqn{\beta}{beta} is a shape parameter, by default this is 1.}
  \item{param}{Parameters of the normal Laplace distribution.}
  \item{xlab, ylab, main}{Plot labels.}
  \item{plot.it}{Logical. Should the result be plotted?}
  \item{line}{Add line through origin with unit slope.}
  \item{\dots}{Further graphical parameters.}
}

\value{
  For \code{qqnl} and \code{ppnl}, a list with components:
  \item{x}{The x coordinates of the points that are to be plotted.}
  \item{y}{The y coordinates of the points that are to be plotted.}
}

\references{
  Wilk, M. B. and Gnanadesikan, R. (1968)
  Probability plotting methods for the analysis of data.
  \emph{Biometrika}.
  \bold{55}, 1--17.
}

\seealso{
  \code{\link{ppoints}}, \code{\link{dnl}}, \code{\link{nlFit}}
}

\examples{
par(mfrow = c(1, 2))
param <- c(2, 2, 1, 1)
y <- rnl(200, param = param)
qqnl(y, param = param, line = FALSE)
abline(0, 1, col = 2)
ppnl(y, param = param)
}

\keyword{nlplot}
\keyword{distribution}
