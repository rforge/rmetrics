\name{gap.test}
\alias{gap.test}
\title{the Gap test}
\description{
  The Gap test for testing random number generators.
}
\usage{
gap.test(u, lower = 0, upper = 1/2, echo = TRUE)
}
\arguments{
  \item{u}{sample of random numbers in ]0,1[.}
  \item{lower}{numeric for the lower bound, default \code{0}.}
  \item{upper}{numeric for the upper bound, default \code{1/2}.}
  \item{echo}{logical to plot detailed results, default \code{TRUE}}
}
\details{
	We consider a vector \code{u}, realisation of i.i.d. uniform random
	variables \eqn{U_1, \dots, U_n}{U1... Un}.

	The gap test works on the 'gap' variables defined as
	\deqn{ G_ i = 
	       	\left\{  
		\begin{array}{cl}
	       	 1 & \textrm{if~} lower \leq U_i \leq upper\\
		 0 & \textrm{otherwise}\\
		 \end{array}
		 \right.
		}{%
		1 if lower <= Ui <= upper, 0 otherwise.
		}
	Let \eqn{p} the probability that \eqn{G_i}{Gi} equals to one.
	Then we compute the length of zero gaps and denote by \eqn{n_j}{nj} the number
	of zero gaps of length \eqn{j}. The chi-squared statistic is given by
	\deqn{ S = \sum_{j=1}^m \frac{(n_j - n p_j)^2}{n p_j},
		}{%
		S =  \sum_{j=0}^m (n_j - n p_j)^2/[n p_j],
		}
	where \eqn{p_j}{pj} stands for the probability the length of zero gaps equals 
	to \eqn{j} (\eqn{ (1-p)^2 p^j
	   	   }{%
		   (1-p)^2 p^j
		   }) and \eqn{m} the max number of lengths (at least
	\eqn{\left\lfloor \frac{ \log( 10^{-1} ) - 2\log(1- p)-log(n) }{ \log( p )} \right\rfloor
		}{%
		floor( ( log( 10^(-1) ) - 2log( 1-p )-log(n) ) / log( p )} ).
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
    \code{\link{order.test}} and \code{\link{coll.test}}

    \code{\link{ks.test}} for the Kolmogorov Smirnov test and \code{\link{acf}} for
    the autocorrelation function. 
}
\author{
 Christophe Dutang.
}
\examples{
# (1) 
#
gap.test(runif(1000))
print( gap.test( runif(1000000), echo=FALSE ) )

# (2) 
#
gap.test(runif(1000), 1/3, 2/3)


}
\keyword{distribution}
