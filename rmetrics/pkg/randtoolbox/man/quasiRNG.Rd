\name{quasi.randtoolbox}
\alias{quasi.randtoolbox}
\alias{torus}
\alias{sobol}
\alias{runif.sobol}
\alias{halton}
\alias{runif.halton}
\title{Toolbox for quasi random number generation}
\description{
	the Torus algorithm, the Sobol and Halton sequences.
}
\usage{
halton(n, dim = 1, init = TRUE, normal = FALSE, usetime = FALSE)
sobol(n, dim = 1, init = TRUE, scrambling = 0, seed = 4711, normal = FALSE)
torus(n, dim = 1, prime, init = TRUE, mixed = FALSE, usetime = FALSE, normal=FALSE)
}
\arguments{
  \item{n}{number of observations. If length(n) > 1, 
  	the length is taken to be the required number.}
  \item{dim}{dimension of observations default 1.}
  \item{init}{ a logical, if TRUE the sequence is initialized and 
	restarts, otherwise not. By default TRUE.}
  \item{normal}{a logical if normal deviates are needed, default FALSE}
  \item{scrambling}{
	an integer value, if 1, 2 or 3 the sequence is scrambled
       otherwise not. If 1, Owen type type of scrambling is 
       applied, if 2, Faure-Tezuka type of scrambling, is
       applied, and if 3, both Owen+Faure-Tezuka type of
       scrambling is applied. By default 0.}
  \item{seed}{
        an integer value, the random seed for initialization
        of the scrambling process. By default 4711. On effective
        if \code{scrambling>0}.
        }
  \item{prime}{a single prime number or a vector of prime numbers to be used
  		 in the Torus sequence. (optional argument).}
  \item{mixed}{a logical to use the mixed Torus algorithm, default FALSE.}
  \item{usetime}{a logical to use the machine time to start the Torus sequence, 
  		   default TRUE. if FALSE, the Torus sequence start from the first term.}
}
\details{

The currently available generator are given below.
\describe{
    \item{\bold{Torus algorithm:}}{
	The \eqn{k}th term of the Torus algorithm in d dimension is given by
	    	\deqn{u_k = \left(frac(k \sqrt{p_1}), ..., frac(k \sqrt{p_d}) \right)}{%
			    u_k = (frac(k sqrt(p_1)), ..., frac(k sqrt(p_d)) )}
	where \eqn{p_i} denotes the ith prime number, \eqn{frac} the fractional part
	(i.e. \eqn{frac(x) = x-floor(x)}). We use the 100 000 first prime numbers
	from \url{http://primes.utm.edu/}, thus the dimension is limited to 100 000. 
	If the user supplys prime numbers through
	the argument \code{prime}, we do NOT check for primality and we cast numerics
	to integers, (i.e. \code{prime=7.1234} will be cast to \code{prime=7} before
	computing Torus sequence).
	The Torus sequence starts from \eqn{k=1} when initialized with 
	\code{init = TRUE} and no not depending on machine time 
	\code{usetime = FALSE}. This is the default. When {init = FALSE}, the sequence
	is not initialized (to 1) and starts from the last term. We can also use the
	machine time to start the sequence with \code{usetime = TRUE}, which overrides
	\code{init}.}

    \item{\bold{scrambled Sobol sequences}}{
	Calculates a matrix of uniform and normal deviated Sobol low 
    	discrepancy numbers. Optional scrambling of the sequence can 
    	be selected.
	The Sobol sequence restarts and is initialized when 
	\code{init = TRUE} and otherwise not.
	}

    \item{\bold{Halton sequences}}{
    	Calculates a matrix of uniform or normal deviated halton low 
    	discrepancy numbers. Let us note that Halton sequence in dimension is the
	Van Der Corput sequence.
	The Halton sequence starts from \eqn{k=1} when initialized with 
	\code{init = TRUE} and no not depending on machine time 
	\code{usetime = FALSE}. This is the default. When {init = FALSE}, the sequence
	is not initialized (to 1) and starts from the last term. We can also use the
	machine time to start the sequence with \code{usetime = TRUE}, which overrides
	\code{init}.
	}	

}%end of describe
See the pdf vignette for details.
}
\value{
  \code{torus}, \code{halton} and \code{sobol} generates random variables in ]0,1[. It returns a \eqn{n}x\eqn{dim} matrix, when \code{dim}>1 otherwise a vector of length \code{n}.
}
\references{

Bratley P., Fox B.L. (1988);
    \emph{Algorithm 659: Implementing Sobol's Quasirandom
    Sequence Generator},
    ACM Transactions on Mathematical Software 14, 88--100.

Joe S., Kuo F.Y. (1998);
    \emph{Remark on Algorithm 659: Implementing Sobol's Quaisrandom
    Seqence Generator}.

Planchet F., Jacquemin J. (2003), \emph{L'utilisation de methodes de 
simulation en assurance}. Bulletin Francais d'Actuariat, vol. 6, 11, 3-69. (available online)

}
\seealso{
    \code{\link{pseudo.randtoolbox}} for pseudo random number generation, \code{\link{.Random.seed}} for what is done in R about random number generation. 
}
\author{
 Christophe Dutang and Diethelm Wuertz
}
\examples{
# (1) the Torus algorithm
#
torus(100)

# example of setting the seed
setSeed(1)
torus(5)
setSeed(6)
torus(5)
#the same
setSeed(1)
torus(10)

#no use of the machine time
torus(10, use=FALSE)

#Kolmogorov Smirnov test
#KS statistic should be around 0.0019
ks.test(torus(1000), punif) 
	
#KS statistic should be around 0.0003
ks.test(torus(10000), punif) 

#the mixed Torus sequence
torus(10, mix=TRUE)
par(mfrow = c(1,2))
acf(torus(10^6))
acf(torus(10^6, mix=TRUE))

# (2) Halton sequences
#

# uniform variate
halton(n = 10, dim = 5)

# normal variate
halton(n = 10, dim = 5, normal = TRUE)

# some plots
par(mfrow = c(2, 2), cex = 0.75)
hist(halton(n = 5000, dim = 1), main = "Uniform Halton", 
  xlab = "x", col = "steelblue3", border = "white")

hist(halton(n = 5000, dim = 1), main = "Normal Halton", 
  xlab = "x", col = "steelblue3", border = "white")
   
# (3) Sobol sequences
#

# uniform variate
sobol(n = 10, dim = 5, scrambling = 3)

# normal variate
sobol(n = 10, dim = 5, scrambling = 3, normal = TRUE)

# some plots
hist(sobol(5000, 1, scrambling = 2), main = "Uniform Sobol", 
  xlab = "x", col = "steelblue3", border = "white")

hist(sobol(5000, 1, scrambling = 2, normal = TRUE), main = "Normal Sobol", 
  xlab = "x", col = "steelblue3", border = "white")


# (4) computation times on my macbook, mean of 1000 runs
#

\dontrun{
# algorithm			time in seconds for n=10^6
# Torus algo					0.058 
# mixed Torus algo 	       			0.087 
# Halton sequence				0.878
# Sobol sequence				0.214
n <- 1e+06
mean( replicate( 1000, system.time( torus(n), gcFirst=TRUE)[3]) )
mean( replicate( 1000, system.time( torus(n, mixed=TRUE), gcFirst=T)[3]) )
mean( replicate( 1000, system.time( halton(n), gcFirst=TRUE)[3]) )
mean( replicate( 1000, system.time( sobol(n), gcFirst=TRUE)[3]) )
	}

}
\keyword{distribution}
