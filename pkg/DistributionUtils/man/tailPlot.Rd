\name{tailPlot}
\alias{tailPlot}
\alias{tailPlotLine}
\alias{normTailPlotLine}
\alias{tTailPlotLine}
\alias{gammaTailPlotLine}

\title{
Tail Plot Functions
}
\description{
Create a left or right tail plot of a data set using
\code{tailPlot}. Add a line for any distribution with parameters given
by an argument named \code{param}, using \code{tailPlotLine}.
Add normal, \eqn{t}, or gamma distribution lines to
the plot using \code{normTailPlotLine}, \code{tTailPlotLine}, or
\code{gammaTailPlotLine}
}
\usage{
tailPlot(x, log = "y", side = c("right", "left"), main = NULL,
         xlab = NULL, ylab = NULL, ...)
tailPlotLine(x, distrFn, param = NULL, side = c("right", "left"), ...)
normTailPlotLine(x, mean = 0, sd = 1, side = c("right", "left"), ...)
tTailPlotLine(x, df = Inf, side = c("right", "left"), ...)
gammaTailPlotLine(x, shape = 1, rate = 1, scale = 1/rate,
                  side = c("right", "left"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector of values for which the tail plot is to be drawn.}
  \item{log}{A character string which contains \code{"x"} if the x-axis
    is to be logarithmic, \code{"y"} if the y-axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}
  \item{side}{Character. \code{"right"} (the default) for a tail plot of
    the right-hand tail, \code{"left"} for a tail plot of
    the left-hand tail.}
  \item{main}{A main title for the plot.}
  \item{xlab}{A label for the x axis, defaults to \code{NULL}.}
  \item{ylab}{A label for the y axis, defaults to \code{NULL}.}
  \item{distrFn}{Character. The name of the distribution function to be
    to be added to the tail plot.}
  \item{param}{Vector specifying the parameters of the distribution,
    defaults to \code{NULL}.}
  \item{mean}{The mean of the normal distribution.}
  \item{sd}{The standard deviation of the normal distribution. Must be
    positive.}
  \item{df}{The degrees of freedom of the \eqn{t}-distribution, (\eqn{>
      0}, may be non-integer). Defaults to \code{Inf}, corresponding to
    the standard normal distribution. }
  \item{shape}{The shape parameter of the gamma distribution. Must be
    positive.}
  \item{scale}{The scale parameter of the gamma distribution. Must be
    strictly positive, \code{scale} strictly.}
  \item{rate}{The rate parameter of the gamma distribution. An
    alternative way to specify the scale.}
  \item{\dots}{Other graphical parameters (see \code{\link{par}}.}
}
\details{
  \code{tailPlot} draws either a left-hand or right-hand tail plot of
  the data \code{x}. See for example Resnick (2007), p.105. The
  left-hand tail plot plots the empirical distribution of the data
  against the order statistics, for order statistic values below the
  median. The right-hand tail plot plots one minus the empirical
  distribution of the data against the order statistics, for order
  statistic values above the median. The default is for the y-axis to be
  plotted on a log scale.

  \code{tailPlotLine} adds a line for the specified distribution to an
  already drawn tail plot. The distribution can be any distribution
  which has default parameters, but if parameters need to be supplied
  the distribution must have an argument \code{param} which specifies
  the parameters. This is the case for all distributions in the form
  recommended in Scott \emph{et al} (2009) and includes distributions
  from the packages \pkg{GeneralizedHyperbolic}, \pkg{SkewHyperbolic}
  and \code{VarianceGamma}.

  \code{normTailPlotLine},\code{tTailPlotLine} and
  \code{gammaTailPlotLine} add the corresponding line
  derived respectively from the given normal, \eqn{t}, or gamma
  distribution to an already drawn tail plot.
}
\value{Returns \code{NULL} invisibly.
}
\references{
  Aas, Kjersti and \enc{Hobæk}{Hobaek} Haff, Ingrid (2006)
  The generalised hyperbolic skew Student's \eqn{t}-distribution.
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.

  Resnick, S. (2007) \emph{Heavy-Tail Phenomena}, New York: Springer.

  Scott, David J. and \enc{Würtz}{Wuertz}, Diethelm and Dong, Christine
  (2009) Software for Distributions in R. UseR: The R User
  Conference 2009
  \url{http://www.agrocampus-ouest.fr/math/useR-2009/slides/Scott+Wuertz+Dong.pdf}
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}
\examples{
### Draw tail plot of some data
x <- rnorm(100, 1, 2)
tailPlot(x)
### Add normal distribution line
normTailPlotLine(x, mean = 1, sd = 2)
### Add t distribution line
tTailPlotLine(x, df = 5, lty = 2)
### Use fitted values
normTailPlotLine(x, mean = mean(x), sd = sd(x), lty = 3)

### Gamma distribution
x <- rgamma(100, shape = 1, scale = 1)
tailPlot(x)
### Add gamma distribution line
gammaTailPlotLine(x, shape = 1, scale = 1)
### Left tail example
tailPlot(x, side = "l")
### Add gamma distribution line
gammaTailPlotLine(x, shape = 1, scale = 1, side = "l")
### Log scale on both axes
tailPlot(x, side = "l", log = "xy")
### Add gamma distribution line
gammaTailPlotLine(x, shape = 1, scale = 1, side = "l")

### Add line from a standard distribution with default parameters
x <- rlnorm(100)
tailPlot(x)
tailPlotLine(x, distrFn = "lnorm")

### Add line from a distribution with 'param' argument
require(VarianceGamma)
param <- c(0,0.5,0,0.5)
x <- rvg(100, param = param)
tailPlot(x)
tailPlotLine(x, distrFn = "vg", param = param) 

}
\keyword{distribution}
\keyword{univar}