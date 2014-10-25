\name{GIGPlots}
\alias{qqgig}
\alias{ppgig}

\title{Generalized Inverse Gaussian Quantile-Quantile and Percent-Percent Plots}

\description{
  \code{qqgig} produces a generalized inverse Gaussian QQ plot of the
  values in \code{y}.

  \code{ppgig} produces a generalized inverse Gaussian PP (percent-percent) or
  probability plot of the values in \code{y}.

  If \code{line = TRUE}, a line with zero intercept and unit slope is
  added to the plot.

  Graphical parameters may be given as arguments to \code{qqgig}, and
  \code{ppgig}.
}
\usage{
qqgig(y, chi = 1, psi = 1, lambda = 1,
      param = c(chi, psi, lambda),
      main = "GIG Q-Q Plot",
      xlab = "Theoretical Quantiles",
      ylab = "Sample Quantiles",
      plot.it = TRUE, line = TRUE, \dots)

ppgig(y, chi = 1, psi = 1, lambda = 1,
      param = c(chi, psi, lambda),
      main = "GIG P-P Plot",
      xlab = "Uniform Quantiles",
      ylab = "Probability-integral-transformed Data",
      plot.it = TRUE, line = TRUE, \dots)
}

\arguments{
  \item{y}{The data sample.}
  \item{chi}{A shape parameter that by default holds a value of 1.}
  \item{psi}{Another shape parameter that is set to 1 by default.}
  \item{lambda}{Shape parameter of the GIG distribution. Common to all forms of 
    parameterization. By default this is set to 1.}
  \item{param}{Parameters of the generalized inverse Gaussian distribution.}
  \item{xlab, ylab, main}{Plot labels.}
  \item{plot.it}{Logical.  TRUE denotes the results should be plotted.}
  \item{line}{Logical. If TRUE, a line with zero intercept and unit slope
    is added to the plot.}
  \item{\dots}{Further graphical parameters.}
}

\value{
  For \code{qqgig} and \code{ppgig}, a list with components:
  \item{x}{The x coordinates of the points that are be plotted.}
  \item{y}{The y coordinates of the points that are be plotted.}
}

\references{
  Wilk, M. B. and Gnanadesikan, R. (1968)
  Probability plotting methods for the analysis of data.
  \emph{Biometrika}.
  \bold{55}, 1--17.
}
\seealso{
  \code{\link{ppoints}}, \code{\link{dgig}}.
}
\examples{
par(mfrow = c(1, 2))
y <- rgig(1000, param = c(2, 3, 1))
qqgig(y, param = c(2, 3, 1), line = FALSE)
abline(0, 1, col = 2)
ppgig(y, param = c(2, 3, 1))
}
\keyword{hplot}
\keyword{distribution}
