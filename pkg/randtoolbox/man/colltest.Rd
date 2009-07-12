\name{coll.test}
\alias{coll.test}
\title{the Collision test}
\description{
  The Collision test for testing random number generators.
}
\usage{
coll.test(rand, lenSample = 2^14, nbCell = 2^20, nbSample = 1000, echo = TRUE, ...)
}
\arguments{
  \item{rand}{a function generating random numbers. its first argument must be
    the 'number of observation' argument as in \code{runif}.}
  \item{lenSample}{numeric for the length of generated samples.}
  \item{nbCell}{numeric for the number of cells in which to put random numbers.}
  \item{nbSample}{numeric for the overall sample number.}
  \item{echo}{logical to plot detailed results, default \code{TRUE}}
  \item{...}{further arguments to pass to function rand}
  
}
\details{
	We consider outputs of multiple calls to a random number generator \code{rand}.  
         Let us denote by \eqn{n} the length of samples (i.e. \code{lenSample} argument),
          \eqn{k} the number of cells (i.e. \code{nbCell} argument) and
         \eqn{m} the number of samples (i.e. \code{nbSample} argument).
         

        A collision is defined as
        when a random number falls in a cell where there are
        already random numbers. Let us note \eqn{C} the number of collisions

        The distribution of collision number \eqn{C} is given by 
        \deqn{
            P(C = c) = \prod_{i=0}^{n-c-1}\frac{k-i}{k} \frac{1}{k^c} \,_2S_n^{n-c}, 
        }{%
            P(C = c) = \prod_{i=0}^{n-c-1} (k-i)/k *1/(k^c) 2S_n^{n-c}, 
        }
        where \eqn{\,_2S_n^k}{2S_n^{n-c}} denotes the Stirling number of the second kind
        and \eqn{c=0,\dots,n-1}{c=0,..., n-1}.

        But we cannot use this formula for large \eqn{n} since the Stirling number
        need \eqn{O(n\log(n))}{O(n log(n))} time to be computed. We use
        a Gaussian approximation if 
        \eqn{\frac{n}{k}>\frac{1}{32}}{ n/k>\frac{1}{32} } and \eqn{n\geq 2^8}{n >= 2^8},
        a Poisson approximation if \eqn{\frac{n}{k} < \frac{1}{32}}{n/k < 1/32} and the exact formula
        otherwise.
        
        Finally we compute \eqn{m} samples of random numbers, on which we calculate
        the number of collisions. Then we are able to compute a chi-squared statistic.
        
	
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
    \code{\link{order.test}} and \code{\link{gap.test}}

    \code{\link{ks.test}} for the Kolmogorov Smirnov test and \code{\link{acf}} for
    the autocorrelation function.
}
\author{
 Christophe Dutang.
}
\examples{
# (1) poisson approximation
#
coll.test(runif)

# (2) exact distribution
#
coll.test(SFMT, 2^7, 2^10, 10000)

}
\keyword{htest}
