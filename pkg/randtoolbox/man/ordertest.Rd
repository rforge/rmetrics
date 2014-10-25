\name{order.test}
\alias{order.test}
\title{the Order test}
\description{
  The Order test for testing random number generators.
}
\usage{
order.test(u, d = 3, echo = TRUE)
}
\arguments{
  \item{u}{sample of random numbers in ]0,1[.}
  \item{echo}{logical to plot detailed results, default \code{TRUE}}
  \item{d}{a numeric for the dimension, see details. When necessary 
  we assume that \code{d} is a multiple of the length of \code{u}.}
  
}
\details{
	We consider a vector \code{u}, realisation of i.i.d. uniform random
	variables \eqn{U_1, \dots, U_n}{U1... Un}.

	The Order test works on a sequence of d-uplets (\eqn{x,y,z} when
         \code{d=3}) of uniform i.i.d. 
	random variables. The triplet is build from the vector \eqn{u}. The number of 
	permutation among the components of a triplet is \eqn{3!=6}, i.e. \eqn{x<y<z},
	\eqn{x<z<y}, \eqn{y<x<z}, \eqn{y<z<x}, \eqn{z<x<y} and \eqn{z<y<x}. The
	Marsaglia test computes the empirical of the different permutations as well
	as the theoretical one \eqn{n/6} where \eqn{n} is the number of triplets.
	Finally the chi-squared statistic is 
	\deqn{ S = \sum_{j=0}^{6} \frac{n_j - n/6)^2}{n/6}.
		}{%
		S =  \sum_{j=1}^6 [n_j - n/6 ]^2/[n/6].
		}
}
\value{
  a list with the following components :

\code{statistic}	 the value of the chi-squared statistic.

\code{p.value}	 	 the p-value of the test.

\code{observed}	 the observed counts.

\code{expected}	 the expected counts under the null hypothesis.

\code{residuals}	 the Pearson residuals, (observed - expected) / sqrt(expected).

}
\references{
Planchet F., Jacquemin J. (2003), \emph{L'utilisation de methodes de 
simulation en assurance}. Bulletin Francais d'Actuariat, vol. 6, 11, 3-69. (available online)


L'Ecuyer P. (2001), \emph{Software for uniform random number
generation distinguishing the good and the bad}. Proceedings of the 2001 
Winter Simulation Conference. (available online)


L'Ecuyer P. (2007), \emph{Test U01: a C library for empirical testing of 
random number generators.} ACM Trans. on Mathematical 
Software 33(4), 22.
}
\seealso{
    other tests of this package \code{\link{freq.test}}, \code{\link{serial.test}}, \code{\link{poker.test}},
    \code{\link{gap.test}} and \code{\link{coll.test}}

    \code{\link{ks.test}} for the Kolmogorov Smirnov test and \code{\link{acf}} for
    the autocorrelation function.
}
\author{
 Christophe Dutang.
}
\examples{
# (1) mersenne twister vs torus
#
order.test(runif(6000))
order.test(torus(6000))

# (2) 
#
order.test(runif(4000), 4)
order.test(torus(4000), 4)

# (3) 
#
order.test(runif(5000), 5)
order.test(torus(5000), 5)


}
\keyword{htest}
