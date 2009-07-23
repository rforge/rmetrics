\name{gigCheckPars}
\alias{gigCheckPars}

\title{Check Parameters of the Generalized Inverse Gaussian Distribution}
\description{
  Given a putative set of parameters for the generalized inverse Gaussian
  distribution, the functions checks if they are in the correct range,
  and if they correspond to the boundary cases.
}
\usage{
gigCheckPars(Theta, ...)
}
\arguments{
  \item{Theta}{Numeric. Putative parameter values for a generalized
    inverse Gaussian distribution.}
  \item{...}{Further arguments for calls to \code{all.equal}.}
}
\details{
  The vector \code{Theta} takes the form \code{c(lambda,chi,psi)}.

  If either \code{chi} or \code{psi} is negative, an error is returned.

  If \code{chi} is 0 (to within tolerance allowed by \code{all.equal})
  then \code{psi} and \code{lambda} must be positive or an error is
  returned. If these conditions are satisfied, the distribution is
  identified as a gamma distribution.

  If \code{psi} is 0 (to within tolerance allowed by \code{all.equal})
  then \code{chi} must be positive and \code{lambda} must be negative or
  an error is returned. If these conditions are satisfied, the
  distribution is identified as an inverse gamma distribution.

  If both \code{chi} and \code{psi} are positive, then the distribution
  is identified as a normal generalized inverse Gaussian distribution.
}

\value{
  A list with components:
  \item{case}{Whichever of \code{'error'}, \code{'gamma'},
    \code{invgamma}, or \code{'normal'} is identified by the function.}
  \item{errMessage}{An appropriate error message if an error was found,
    the empty string \code{""} otherwise.}
}

\references{
  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley
}
\author{David Scott \email{d.scott@auckland.ac.nz}}
\seealso{\code{\link{dgig}}}
\examples{
gigCheckPars(c(-0.5,5,2.5))      # normal
gigCheckPars(c(0.5,-5,2.5))      # error
gigCheckPars(c(0.5,5,-2.5))      # error
gigCheckPars(c(0.5,-5,-2.5))     # error
gigCheckPars(c(0.5,0,2.5))       # gamma
gigCheckPars(c(-0.5,0,2.5))      # error
gigCheckPars(c(0.5,0,0))         # error
gigCheckPars(c(-0.5,0,0))        # error
gigCheckPars(c(0.5,5,0))         # error
gigCheckPars(c(-0.5,5,0))        # invgamma
}
\keyword{distribution}\name{gigCheckPars}
