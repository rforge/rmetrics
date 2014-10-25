\name{auxiliary}
\alias{auxiliary}
\alias{stirling}
\alias{permut}
\title{Auxiliary functions for 'randtoolbox' package.}
\description{
    Stirling numbers of the second kind and permutation of positive integers.
}
\usage{
stirling(n)
permut(n)
}
\arguments{
  \item{n}{a positive integer.}
}
\details{
        \code{stirling} computes stirling numbers of second kind i.e.
        \deqn{Stirl_n^k = k * Stirl_{n-1}^k + Stirl_{n-1}^{k-1}}
        with \eqn{Stirl_n^1 = Stirl_n^n = 1}.
        e.g. 
	\itemize{
        \item{}{\eqn{n = 0}, returns 1}
        \item{}{\eqn{n = 1}, returns a vector with 0,1}
        \item{}{\eqn{n = 2}, returns a vector with 0,1,1}
        \item{}{\eqn{n = 3}, returns a vector with 0,1,3,1}
        \item{}{\eqn{n = 4}, returns a vector with 0,1,7,6,1...}
	}
        Go to wikipedia for more details.
	
	\code{permut} compute permutation of \eqn{{1, ..., n}} and store it
	in a matrix.
	e.g. 
	\itemize{
	\item{}{\eqn{n=1}, returns a matrix with \tabular{c}{1}}
	\item{}{\eqn{n=2}, returns a matrix with \tabular{cc}{
	1 \tab 2 \cr
	2 \tab 1 \cr}}
	\item{}{\eqn{n=3} returns a matrix with \tabular{ccc}{
	3 \tab 1 \tab 2\cr
	3 \tab 2 \tab 1\cr
	1 \tab 3 \tab 2\cr
	2 \tab 3 \tab 1\cr
	1 \tab 2 \tab 3\cr
	2 \tab 1 \tab 3\cr}}
	}
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

