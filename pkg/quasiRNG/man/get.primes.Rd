\name{get.primes}
\alias{get.primes}
\concept{prime number}
\title{Get primes for quasi random number generation}
\description{
	Provides a vector of a specified number of smallest primes from the
    internal table of the package.
}
\usage{
get.primes(n)
}
\arguments{
  \item{n}{The required number of primes. Should be at most 100 000.}
}
\details{
  The package contains an internal table of the smallest 100 000 primes,
  which may be used in \code{torus} algorithm.
}
\value{
  Vetor of \code{min(n, 100000)} smallest primes.
}
\seealso{
    \code{\link{torus}}
}
\examples{
p <- get.primes(20)
torus(5,dim=10,prime=p[11:20])
}

