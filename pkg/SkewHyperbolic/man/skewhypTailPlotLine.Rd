\name{skewhypTailPlotLine}
\alias{skewhypTailPlotLine}

\title{
Tail Plot Line
}
\description{
Adds skew hyperbolic \emph{t}-distribution line to a tail plot
}
\usage{
skewhypTailPlotLine(x, mu = 0, delta = 1, beta = 1, nu = 1,
                    param = c(mu,delta,beta,nu),
                    side = c("right", "left"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of values for which the tail plot has been drawn.}
  \item{side}{Character. \code{"right"} (the default) if the tail plot
    is of the right-hand tail, \code{"left"} if the tail plot is of the
    left-hand tail.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Scale parameter \eqn{\delta}{delta}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
  \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,delta,beta,nu)}.}
  \item{\dots}{Other graphical parameters (see \code{\link{par}}.}
}
\details{
  The function \code{tailPlot} from \pkg{DistributionUtils} can be used
  to draw either a left-hand or right-hand tail plot of
  the data \code{x}. See for example Resnick (2007), p.105. The
  left-hand tail plot plots the empirical distribution of the data
  against the order statistics, for order statistic values below the
  median. The right-hand tail plot plots one minus the empirical
  distribution of the data against the order statistics, for order
  statistic values above the median. The default is for the y-axis to be
  plotted on a log scale.

  \code{skewhypTailPlotLine} adds the line derived from the given skew
  hyperbolic \emph{t}-distribution to an already drawn tail plot.
}
\value{Returns \code{NULL} invisibly.
}
\references{
  Aas, Kjersti and \enc{Hobæk}{Hobaek} Haff, Ingrid (2006)
  The generalised hyperbolic skew Student's \eqn{t}-distribution.
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.

  Resnick, S. (2007) \emph{Heavy-Tail Phenomena}, New York: Springer.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}
\seealso{
  \code{\link[DistributionUtils]{tailPlot}} and
  \code{\link{skewhypFit}}.
}
\examples{
### Draw tail plot of some data
param <- c(0,1,1,10)
x <- rskewhyp(200, param = param)
tailPlot(x)
### Add skew hyperbolic t-distribution line
skewhypTailPlotLine(x, param = param)
### Parameters from fit may look better
paramFit <- skewhypFit(x, plots = FALSE)$param
tailPlot(x)
skewhypTailPlotLine(x, param = param)
skewhypTailPlotLine(x, param = paramFit, col = "steelblue")

### Left tail example
tailPlot(x, side = "l")
### Add skew hyperbolic t-distribution line
skewhypTailPlotLine(x, param = paramFit, side = "l")
### Log scale on both axes
tailPlot(x, side = "r", log = "xy")
### Add skew hyperbolic t-distribution line
skewhypTailPlotLine(x, param = paramFit, side = "r")

}
\keyword{distribution}
\keyword{univar}