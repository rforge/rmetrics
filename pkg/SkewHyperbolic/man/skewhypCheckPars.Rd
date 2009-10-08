\name{skewhypCheckPars}
\alias{skewhypCheckPars}
\title{Check Parameters of the Skew Hyperbolic Student t-distribution}
\description{
  Given a set of parameters for the skew hyperbolic Student
  \emph{t}-distribution, the function checks that the parameters are in
  the correct range, and that the set has the correct length of 4.
}
\usage{
skewhypCheckPars(param)
}
\arguments{
  \item{param}{A numeric vector of proposed parameters for the skew hyperbolic
    \emph{t}-distribution.}
}
\details{
  The vector \code{param} should be of the form
  \code{c(mu,delta,beta,nu)}.
  If either \code{delta} or \code{nu} is not greater than zero an error
  message is returned.
  If the vector \code{param} does not have a length of 4 then an error
  message is returned.
}
\value{
  A list with components:
  \item{case}{Either \code{'error'} or \code{'normal'}, as identified by
    the function.}
  \item{errMessage}{An appropriate error message if an error was found,
    otherwise an empty string.}
}
\seealso{
  \code{\link{dskewhyp}}
}
\examples{
skewhypCheckPars(c(0,1,1,1))     #normal
skewhypCheckPars(c(0,0,1,1))     #error
skewhypCheckPars(c(0,1,1,-1))    #error
skewhypCheckPars(c(0,1,1))       #error
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\keyword{distribution}
