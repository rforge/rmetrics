\name{poker.test}
\alias{poker.test}
\title{the Poker test}
\description{
  The Poker test for testing random number generators.
}
\usage{
poker.test(u , nbcard = 5, echo = TRUE)
}
\arguments{
  \item{u}{sample of random numbers in ]0,1[.}
  \item{echo}{logical to plot detailed results, default \code{TRUE}}
  \item{nbcard}{a numeric for the number of cards,  
  we assume that the length of \code{u} is a multiple of \code{nbcard}.}
  
}
\details{
        We consider a vector \code{u}, realisation of i.i.d. uniform random
	variables \eqn{U_1, \dots, U_n}{U1... Un}.
	
        Let us note \eqn{k} the card number (i.e. \code{nbcard}).
	The poker test computes a serie of 'hands' in \eqn{\{0, \dots, k-1\}}{\{0, ..., k-1\}}
	from the sample \eqn{h_i = \lfloor u_i d\rfloor}{
	u_i = floor(u_i k)} (\code{u} must have a length dividable by \eqn{k}). Let
	\eqn{n_j} be the number of 'hands' with (exactly) \eqn{j} different cards. The
	probability is \deqn{ p_j = \frac{k!}{k^k (k-j)!* S_k^j} 
	* (\frac{j}{k})^(k-j),  }{%
	p_j = 1/k^k * k! / (k-j)!) * S_k^j, }
	where \eqn{S_k^j} denotes the Stirling numbers of the second kind. Finally the 
	chi-squared statistic is 
	\deqn{ S = \sum_{j=0}^{k-1} \frac{n_j - np_j/k)^2}{np_j/k}.
		}{%
		S =  \sum_{j=1}^k [n_j - np_j/k ]^2/[np_j/k].
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
    other tests of this package \code{\link{freq.test}}, \code{\link{serial.test}}, \code{\link{gap.test}},
    \code{\link{order.test}} and \code{\link{coll.test}}

    \code{\link{ks.test}} for the Kolmogorov Smirnov test and \code{\link{acf}} for
    the autocorrelation function.
}
\author{
 Christophe Dutang.
}
\examples{
# (1) hands of 5 'cards'
#
poker.test(runif(50000))

# (2) hands of 4 'cards'
#
poker.test(runif(40000), 4)

# (3) hands of 42 'cards'
#
poker.test(runif(420000), 42)

}
\keyword{distribution}
