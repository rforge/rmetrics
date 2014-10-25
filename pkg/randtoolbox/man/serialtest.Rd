\name{serial.test}
\alias{serial.test}
\title{the Serial test}
\description{
  The Serial test for testing random number generators.
}
\usage{
serial.test(u , d = 8, echo = TRUE)
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

	The serial test computes a serie of integer pairs \eqn{(p_i,p_{i+1})} 
	from the sample \code{u} with \eqn{p_i = \lfloor u_i d\rfloor}{
	p_i = floor(u_i d)} (\code{u} must have an even length). 
	Let \eqn{n_j}{n_j} be the number of pairs such that
	\eqn{j=p_i \times d + p_{i+1}}{j=p_i  d + p_{i+1}}. If \code{d=2}, we count
	the number of pairs equals to \eqn{00, 01, 10} and \eqn{11}. Since 
	all the combination of two elements in \eqn{\{0, \dots, d-1\}}{\{0, ..., d-1\}}
	are equiprobable, the chi-squared statistic is
	\deqn{ S = \sum_{j=0}^{d-1} \frac{n_j - n/(2 d^2))^2}{n/(2 d^2)}.
		}{%
		S =  \sum_{j=0}^{d-1} [n_j - n/(2 d^2)]^2/[n/(2 d^2)].
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
    other tests of this package \code{\link{freq.test}}, \code{\link{gap.test}}, \code{\link{poker.test}},
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
serial.test(runif(1000))
print( serial.test( runif(1000000), d=2, e=FALSE) )

# (2) 
#
serial.test(runif(5000), 5) 

}
\keyword{htest}
