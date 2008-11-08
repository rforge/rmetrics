\name{freq.test}
\alias{freq.test}
\title{the Frequency test}
\description{
  The Frequency test for testing random number generators.
}
\usage{
freq.test(u, seq = 0:15, echo = TRUE)
}
\arguments{
  \item{u}{sample of random numbers in ]0,1[.}
  \item{echo}{logical to plot detailed results, default \code{TRUE}}
  \item{seq}{a vector of contiguous integers, default \code{0:15}.}
}
\details{
	We consider a vector \code{u}, realisation of i.i.d. uniform random
	variables \eqn{U_1, \dots, U_n}{U1... Un}.

	
	The frequency test works on a serie \code{seq} of ordered contiguous integers 
	(\eqn{s_1,\dots,s_d}{s_1, ...,s_d}), where \eqn{s_j\in  Z\!\!Z}{s_j in Z}. From the
	sample \code{u}, we compute observed integers as
	\deqn{d_i = \lfloor u_i * ( s_d + 1 ) + s_1 \rfloor,
		  }{%
		  d_i = floor( u_i * ( s_d + 1 ) + s_1 ),} 
	(i.e. \eqn{d_i} are uniformely distributed in
	\eqn{\{s_1,\dots,s_d\}}{{s_1, ...,s_d}}). The expected number of integers equals to 
	\eqn{j} is \eqn{m= \frac{1}{s_d - s_1+1}\times n}{m=n/(s_d - s_1+1)}. Finally, the
	chi-squared statistic is 
 	\deqn{ S = \sum_{j=1}^d \frac{(card(d_i=s_j) - m)^2}{m}.
		}{%
		S =  \sum_{j=1}^d (card(d_i=s_j) - m)^2/m .
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
    other tests of this package \code{\link{gap.test}}, \code{\link{serial.test}}, \code{\link{poker.test}},
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
freq.test(runif(1000))
print( freq.test( runif(1000000), echo=FALSE) )

# (2) 
#
freq.test(runif(1000), 1:4)

freq.test(runif(1000), 10:40)

}
\keyword{distribution}
