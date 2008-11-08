\name{auxiliary}
\alias{auxiliary}
\alias{stirling}
\title{Auxiliary functions for 'randtoolbox' package.}
\description{
    Stirling numbers of the second kind
}
\usage{
stirling(n)
}
\arguments{
  \item{n}{a positive integer.}
}
\details{
        \code{stirling} computes stirling numbers of second kind i.e.
        \deqn{Stirl_n^k = k * Stirl_{n-1}^k + Stirl_{n-1}^{k-1}}
        with \eqn{Stirl_n^1 = Stirl_n^n = 1}.
        e.g. 
        \eqn{n = 0}, returns 1
        \eqn{n = 1}, returns c(0,1)
        \eqn{n = 2}, returns c(0,1,1)
        \eqn{n = 3}, returns c(0,1,3,1)
        \eqn{n = 4}, returns c(0,1,7,6,1)...
        Go to wikipedia for more details.

}
\value{
  a vector with stirling numbers.
}
\seealso{
    \code{\link{choose}} for combination numbers.
}
\author{
 Christophe Dutang.
}
\examples{

# should be 1
stirling(0)

#  should be 0,1,7,6,1
stirling(4)
}
\keyword{distribution}

