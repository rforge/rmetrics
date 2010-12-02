\name{Specific Unimodel Distribution Mode}

\alias{ghypMode}

\title{Mode of the Unimodal Distribution}
\description{
  Function to calculate mode of a unimodal distribution which is
  specified by the root of the density function name and the
  corresponding parameters.
}
\usage{
distMode(densFn, ...)
}
\arguments{
  \item{densFn}{Character. The name of the density function required
    calculating the mode.}
  \item{\dots}{Passes arguments to \code{\link{optimize}}.In
    particular, the parameters of the distribution.}
}

\details{
  The name of the unimodal density function must be supplied as the
  characters of the root for that density (e.g. \code{norm},
  \code{ghyp}). The particular unimodal distribution being considered is
  specified by the value of the parameter value.
}
\value{
  The mode is found by a numerical optimisation using
  \code{\link{optimize}}.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
  Joyce Li \email{xli053@aucklanduni.ac.nz}}

\seealso{
  \code{\link{distStepSize}}, \code{\link{qDist}}.
}

\examples{
normRange <- distCalcRange("norm", tol = 10^(-7), mean = 4, sd = 1)
curve(dnorm(x, mean = 4, sd = 1), normRange[1], normRange[2])
abline(v = distMode("norm", mean = 4, sd = 1), col = "blue")
}
\keyword{distribution}
\keyword{univar}

