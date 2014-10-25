\name{GeneralizedHyperbolicPlots}
\alias{qqghyp}
\alias{ppghyp}

\title{Generalized Hyperbolic Quantile-Quantile and Percent-Percent Plots}

\description{
  \code{qqghyp} produces a generalized hyperbolic Q-Q plot of the values in
	\code{y}.

  \code{ppghyp} produces a generalized hyperbolic P-P (percent-percent) or
  probability plot of the values in \code{y}.	

  Graphical parameters may be given as arguments to \code{qqghyp},
  and \code{ppghyp}.
}
\usage{
qqghyp(y, Theta, main = "Generalized Hyperbolic Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  plot.it = TRUE, line = TRUE, \dots)

ppghyp(y, Theta, main = "Generalized Hyperbolic P-P Plot",
  xlab = "Uniform Quantiles",
  ylab = "Probability-integral-transformed Data",
  plot.it = TRUE, line = TRUE, \dots)
}

\arguments{
  \item{y}{The data sample.}
  \item{Theta}{Parameters of the generalized hyperbolic distribution.}
  \item{xlab, ylab, main}{Plot labels.}
  \item{plot.it}{Logical. Should the result be plotted?}
  \item{line}{Add line through origin with unit slope.}
  \item{\dots}{Further graphical parameters.}
}

\value{
  For \code{qqghyp} and \code{ppghyp}, a list with components:
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
  \code{\link{ppoints}}, \code{\link{dghyp}}.
}


\examples{
par(mfrow = c(1,2))
y <- rghyp(200, c(2,2,1,2,2))
qqghyp(y, c(2,2,1,2,2),line = FALSE)
abline(0, 1, col = 2)
ppghyp(y, c(2,2,1,2,2))
}
\keyword{hplot}
\keyword{distribution}
