\name{true.randtoolbox}
\alias{true.randtoolbox}
\alias{trueRNG}
\title{Compare pseudo and quasi RNG with true randomness}
\description{
	Use the random package to compare pseudo/quasi RNGs with true randomness
}
\usage{
trueRNG(n, dim = 1)
}
\arguments{
  \item{n}{number of observations. If length(n) > 1, 
  		  the length is taken to be the required number.}
  \item{dim}{dimension of observations (must be <=100 000, default 1).}
}
\details{

See \code{randomNumbers} from the random package.

}
\value{
  \code{trueRNG} generates random variables in ]0,1[. It returns a \eqn{n}x\eqn{dim} matrix, when \code{dim}>1 otherwise a vector of length \code{n}.
}
\seealso{
    \code{WELL}, \code{SFMT} for what is done in this package. 
}
\author{
 Christophe Dutang
}
\examples{
require(random)


}
\keyword{distribution}

