\name{distCalcRange}
\alias{distCalcRange}

\title{
  Range of a Unimodal Distribution
}

\description{
  Given the parameters of a unimodal distribution and the root of
  the density function name, this function determines the range outside
  of which the density function is negligible, to a specified
  tolerance.
}

\usage{
distCalcRange(densFn, param = NULL, tol = 10^(-5), ...)
}
\arguments{
  \item{densFn}{Character. The name of the density function for which
    range calculation is required.}
  \item{tol}{Tolerance.}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
    are specified, then the default parameter values of each
    distribution are used instead.}
  \item{\dots}{Passes arguments to \code{\link{uniroot}}.In
    particular, the parameters of the distribution.}
}

\details{
  The name of the unimodal density function must be supplied as the
  characters of the root for that density (e.g. \code{norm},
  \code{ghyp}). The particular unimodal distribution being considered is
  specified by the values of the parameters or of the
  \code{param} vector.

  The function gives a range, outside of which
  the density is less than the given tolerance. It is used in
  determining break points for the separate sections over which
  numerical integration is used to determine the distribution
  function. The points are found by using \code{\link{uniroot}} on the
  density function.
}

\value{
  A two-component vector giving the lower and upper ends of the range.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Joyce Li \email{xli053@aucklanduni.ac.nz}
}

\seealso{
  \code{\link{qDist}}
}

\examples{
normRange <- distCalcRange("norm", tol = 10^(-7), mean = 4, sd = 1)
normRange
tRange <- distCalcRange("t", tol = 10^(-5), df = 4)
tRange
}

\keyword{distribution}
\keyword{univar}