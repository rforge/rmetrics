\name{pseudo.rngWELL}
\alias{pseudo.rngWELL}
\alias{WELL2test}
\alias{setSeed4WELL}
\title{Toolbox for pseudo and quasi random number generation}
\description{
	General linear congruential generators such as Park Miller sequence,
	generalized feedback shift register such as SF-Mersenne Twister algorithm
	and WELL generator; and a quasi random generator 
	(pseudo random generators) and the Torus algorithm (quasi random
	generation).
}
\usage{
WELL2test(n, dim = 1, order = 512, temper = FALSE, version = "a")
setSeed4WELL(seed)
}
\arguments{
  \item{n}{number of observations. If length(n) > 1, 
  		  the length is taken to be the required number.}
  \item{dim}{dimension of observations (must be <=100 000, default 1).}
  \item{seed}{a single value, interpreted as a positive integer for the seed. e.g.
  		append your day, your month and your year of birth.}
  \item{order}{a positive integer for the order of the characteristic polynomial. see details}
  \item{temper}{a logical if you want to do a tempering stage. see details}
  \item{version}{a character either 'a' or 'b'. see details}
}
\details{

The currently available generator are given below.
\describe{
   \item{\bold{WELL generator:}}{
	The WELL (which stands for Well Equidistributed Long-period Linear) is in a sentence
	a generator with better equidistribution than Mersenne Twister algorithm but this
	gain of quality has to be paid by a slight higher cost of time. 
	See Panneton et al. (2006) for	details. 

	The \code{order} argument of \code{WELL}
	generator is the order of the characteristic polynomial, which is denoted by \eqn{k} in 
	Paneton F., L'Ecuyer P. and Matsumoto M. (2006). Possible values for \code{order} 	
	are 512, 521, 607, 1024 where no tempering are needed (thus possible).
 	Order can also be 800, 19937, 21071, 23209, 44497 where a tempering stage
	is possible through the \code{temper} argument. 
	Furthermore a possible 'b' version of WELL RNGs are possible for the 
	following order 521, 607, 1024, 800, 19937, 23209 with the \code{version}
	argument.
	
	All the C code for WELL generator used in this 
	package is the code of P. L'Ecuyer (cf. 
	\url{http://www.iro.umontreal.ca/~lecuyer/}), except
	some C code, we add, to \emph{interface} with R.}


}%end of describe
See the pdf vignette for details.
}
\value{
  \code{WELL2test} generates random variables in [0,1[. It returns a \eqn{n}x\eqn{dim} matrix, when \code{dim}>1 otherwise a vector of length \code{n}.

  \code{setSeed4WELL} set the seed of the \code{rngWELL} package 
(i.e. for the \code{WELL2test} functions).
}
\references{
Paneton F., L'Ecuyer P. and Matsumoto M. (2006), \emph{Improved Long-Period Generators 
Based on Linear Recurrences Modulo 2}, ACM Transactions on Mathematical Software. (preprint
available online)
}
\seealso{
    \code{\link{.Random.seed}} for what is done in R about random number generation. 
}
\author{
 Christophe Dutang and Petr Savicky
}
\examples{
# (1) WELL generator
#

# 'basic' calls
# WELL512
WELL2test(10, order = 512)
# WELL1024
WELL2test(10, order = 1024)
# WELL19937
WELL2test(10, order = 19937)
# WELL44497
WELL2test(10, order = 44497)
# WELL19937 with tempering 
WELL2test(10, order = 19937, temper = TRUE)
# WELL44497 with tempering
WELL2test(10, order = 44497, temper = TRUE)

# tempering vs no tempering
setSeed4WELL(08082008)
WELL2test(10, order =19937)
setSeed4WELL(08082008)
WELL2test(10, order =19937, temper=TRUE)

}
\keyword{distribution}

