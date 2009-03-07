\name{gigCalcRange}
\alias{gigCalcRange}

\title{Range of a Generalized Inverse Gaussian Distribution}
\description{
  Given the parameter vector Theta of a generalized inverse Gaussian
  distribution, this function determines the range outside of which the density
  function is negligible, to a specified tolerance. The parameterization
  used is the \eqn{(\chi,\psi)}{(chi, psi)} one (see
  \code{\link{dgig}}). To use another parameterization, use
  \code{\link{gigChangePars}}.
}

\usage{
gigCalcRange(Theta, tol = 10^(-5), density = TRUE, ...)
}
\arguments{
  \item{Theta}{Value of parameter vector specifying the generalized
    inverse Gaussian distribution.}
  \item{tol}{Tolerance.}
  \item{density}{Logical. If \code{TRUE}, the bounds are for the density
    function. If \code{FALSE}, they should be for the probability
    distribution, but this has not yet been implemented.}
  \item{\dots}{Extra arguments for calls to \code{\link{uniroot}}.}
}
\details{
  The particular generalized inverse Gaussian distribution being
  considered is specified by the value of the parameter value
  \code{Theta}.

  If \code{density = TRUE}, the function gives a range, outside of which
  the density is less than the given tolerance. Useful for plotting the
  density. Also used in determining break points for the separate
  sections over which numerical integration is used to determine the
  distribution function. The points are found by using
  \code{\link{uniroot}} on the density function.

  If \code{density = FALSE}, the function returns the message:
  "\code{Distribution function bounds not yet implemented}".
}
\value{
  A two-component vector giving the lower and upper ends of the range.
}
\references{
  Jorgensen, B. (1982).
  \emph{Statistical Properties of the Generalized Inverse Gaussian
  Distribution}.
  Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York.
}
\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{
  \code{\link{dgig}}, \code{\link{gigChangePars}}
}

\examples{
Theta <- c(-0.5,5,2.5)
maxDens <- dgig(gigMode(Theta), Theta)
gigRange <- gigCalcRange(Theta, tol = 10^(-3)*maxDens)
gigRange
curve(dgig(x, Theta), gigRange[1], gigRange[2])
\dontrun{gigCalcRange(Theta, tol = 10^(-3), density = FALSE)}
}
\keyword{distribution}
