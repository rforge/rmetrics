\name{vgCheckPars}
\alias{vgCheckPars}

\title{Check Parameters of the Variance Gamma Distribution}

\description{
  Given a putative set of parameters for the variance gamma
  distribution, the functions checks if the parameters are in the
  correct range, and if the set has the correct length of 4.  }

\usage{
  vgCheckPars(param, ...)
}

\arguments{
  \item{param}{Numeric. Putative parameter values for a Variance Gamma
    distribution.}
  \item{...}{Further arguments for calls to \code{all.equal}.}
}

\details{  
  The vector \code{param} takes the form \code{c(c, sigma, theta, nu)}.
  If either \code{sigma} or \code{nu} is negative, then an error message
  is returned.

  If the vector \code{param} has a length not equal to 4, then an error
  message is returned.
}

\value{
  A list with components:
  \item{case}{Whichever of \code{'error'} or \code{'normal'} is
    identified by the function.}

  \item{errMessage}{An appropriate error message if an error was found,
    the empty string \code{""} otherwise.}
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{\code{\link{dvg}}, code{\link{vgMom}}}

\reference{}

\examples{
 vgCheckPars(c(0,1,0,1))      # normal
 vgCheckPars(c(0,0,0,1))      # error
 vgCheckPars(c(0,1,0,-2))     # error
 vgCheckPars(c(0,1,0))        # error
}

\keyword{distribution}
