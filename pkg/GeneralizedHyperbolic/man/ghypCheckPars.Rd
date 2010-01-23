\name{ghypCheckPars}
\alias{ghypCheckPars}

\title{Check Parameters of the Generalized Hyperbolic Distribution}
\description{
  Given a putative set of parameters for the generalized hyperbolic
  distribution, the functions checks if they are in the correct range,
  and if they correspond to the boundary cases.
}
\usage{
ghypCheckPars(param)
}
\arguments{
  \item{param}{Numeric. Putative parameter values for a generalized
    hyperblic distribution.}
}
\details{
  The vector \code{param} takes the form \code{c(mu, delta, alpha, beta, lambda)}.

  If \code{alpha} is negative, an error is returned.

  If lambda is 0 then the absolute value of \code{beta} must be
  less than \code{alpha} and delta must be greater than zero. If
  either of these conditions are false, than a error is returned.

  If \code{lambda} is greater than 0 the absolute value of \code{beta} must
  be less than \code{alpha}. \code{delta} must also be non-negative. When
  either one of these is not true, an error is returned.

  If \code{lambda} is less than 0 then the absolute value of \code{beta} must
  be equal to \code{alpha}. \code{delta} must be greater than 0, if
  both conditions are not true, an error is returned.
}

\value{
  A list with components:
  \item{case}{Either \code{""} or \code{"error"}.}
  \item{errMessage}{An appropriate error message if an error was found,
    the empty string \code{""} otherwise.}
}

\references{
  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley
}
\author{David Scott \email{d.scott@auckland.ac.nz}}
\seealso{\code{\link{dghyp}}}
\examples{
ghypCheckPars(c(0, 2.5, -0.5, 1, 0))      # error
ghypCheckPars(c(0, 2.5, 0.5, 0, 0))       # normal
ghypCheckPars(c(0, 1, 1, -1, 0))          # error
ghypCheckPars(c(2, 0, 1, 0.5, 0))         # error
ghypCheckPars(c(0, 5, 2, 1.5, 0))         # normal
ghypCheckPars(c(0, -2.5, -0.5, 1, 1))     # error
ghypCheckPars(c(0, -1, 0.5, 1, 1))        # error
ghypCheckPars(c(0, 0, -0.5, -1, 1))       # error
ghypCheckPars(c(2, 0, 0.5, 0, -1))        # error
ghypCheckPars(c(2, 0, 1, 0.5, 1))         # skew laplace
ghypCheckPars(c(0, 1, 1, 1, -1))          # skew hyperbolic
}
\keyword{distribution}
