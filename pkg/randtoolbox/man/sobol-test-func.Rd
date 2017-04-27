\name{soboltestfunctions}
\alias{sobol.directions}
\alias{sobol.basic}
\alias{sobol.R}
\alias{int2bit}
\alias{bit2int}
\alias{bit2unitreal}
\title{Some test functions for Sobol sequences}
\description{
	Some test functions for Sobol sequences
}
\usage{
sobol.directions(polycoef, deg, prevmj, nbpoint, echo=FALSE)
sobol.basic(n, polycoef, deg, prevmj, echo=FALSE, output=c("real", "integer"))
sobol.R(n, d, echo=FALSE)

int2bit(x)
bit2int(x)
bit2unitreal(x)
}
\arguments{
  \item{n}{number of observations.}
  \item{d}{dimension of observations.}
  \item{polycoef}{binary coefficients of the primitive polynomial.}
  \item{deg}{degree of the primitive polynomial.}
  \item{prevmj}{matrix where each column is the binary representation of integers \eqn{m_j}}
  \item{nbpoint}{number of additionnal integers \eqn{m_j}.}
  \item{echo}{a logical to show some traces.}
  \item{output}{a character string either \code{"real"} or \code{"integer"}
  to specify whether the radical inverse function is computed \code{output="real"}
  or not \code{output="integer"}.}		   
  \item{x}{an integer to convert in base 2 or its binary representation}
}
\details{

\code{sobol.directions} computes the direction numbers used when computing Sobol sequences.

\code{sobol.basic} compute the Sobol sequence in one dimension according to a primitive
polynomial and specified integers \eqn{m_j}.

\code{int2bit} computes the binary representation of the integer part of a real,
\code{bit2int} computes an integer from its binary representation.
\code{bit2unitreal} computes the radical inverse function in base 2.


}
\value{
  a vector of length \code{n} for \code{sobol.basic} or a matrix for \code{sobol.directions}.
}
\references{
Glasserman P., (2003);
    \emph{Monte Carlo Methods in Financial Engineering},
    Springer.
}
\seealso{
    \code{\link{quasiRNG}} for quasi random number generation. 
}
\author{
 Christophe Dutang
}
\examples{
#page 306 of Glassermann
p13 <- int2bit(13)
prevterm <- sapply(c(1,3,3), int2bit)
colnames(prevterm) <- apply(prevterm, 2, bit2int)
sobol.directions(p13, 3, prevterm, 2, echo=FALSE)


sobol.basic(15, p13, 3, c(1,3,3), echo=FALSE)
sobol.basic(15, p13, 3, c(1,3,3), echo=FALSE, output="i")


#page 307 of Glassermann
trueval <- c(16, 24,  8, 12, 28, 20,  4, 30, 14,  
6, 22, 18,  2, 10, 26,  5, 21, 29, 13,  9, 25, 17,  
1, 27, 11,  3, 19, 23,  7, 15, 31)/32
cbind(sobol.basic(31, p13, 3, prevterm, echo=FALSE, output="r") , trueval)


}
\keyword{distribution}
