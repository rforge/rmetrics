\name{SkewLaplacePlots}
\alias{qqskewlap}
\alias{ppskewlap}

\title{Skew-Laplace Quantile-Quantile and Percent-Percent Plots}

\description{
  \code{qqskewlap} produces a skew-Laplace QQ plot of the
  values in \code{y}.

  \code{ppskewlap} produces a skew-Laplace PP (percent-percent) or
  probability plot of the values in \code{y}.

  If \code{line = TRUE}, a line with zero intercept and unit slope is
  added to the plot.

  Graphical parameters may be given as arguments to \code{qqskewlap}, and
  \code{ppskewlap}.
}
\usage{
qqskewlap(y, Theta, main = "Skew-Laplace Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles",
  plot.it = TRUE, line = TRUE, \dots)

ppskewlap(y, Theta, main = "Skew-Laplace P-P Plot",
  xlab = "Uniform Quantiles",
  ylab = "Probability-integral-transformed Data",
  plot.it = TRUE, line = TRUE, \dots)
}

\arguments{
  \item{y}{The data sample.}
  \item{Theta}{Parameters of the skew-Laplace distribution.}
  \item{xlab, ylab, main}{Plot labels.}
  \item{plot.it}{Logical.  TRUE denotes the results should be plotted.}
  \item{line}{Logical. If TRUE, a line with zero intercept and unit slope
    is added to the plot.}
  \item{\dots}{Further graphical parameters.}
}

\value{
  For \code{qqskewlap} and \code{ppskewlap}, a list with components:
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
  \code{\link{ppoints}}, \code{\link{dskewlap}}.
}
\examples{
par(mfrow=c(1,2))
y <- rskewlap(1000,c(0.5,1,2))
qqskewlap(y,c(0.5,1,2),line=FALSE)
abline(0,1,col=2)
ppskewlap(y,c(0.5,1,2))
}
\keyword{hplot}
\keyword{distribution}
