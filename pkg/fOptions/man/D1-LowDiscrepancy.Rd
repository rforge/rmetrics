\name{LowDiscrepancy}

\alias{LowDiscrepancy}

\alias{runif.halton}
\alias{rnorm.halton}
\alias{runif.sobol}
\alias{rnorm.sobol}
\alias{runif.pseudo}
\alias{rnorm.pseudo}


\title{Low Discrepancy Sequences}


\description{
    
    A collection and description of functions to compute 
    Halton's and Sobol's low discrepancy sequences, 
    distributed in form of a uniform or normal distribution. 
    \cr
    
    The functions are:

    \tabular{ll}{
	\code{runif.halton} \tab Uniform Halton sequence, \cr
	\code{rnorm.halton} \tab Normal Halton sequence, \cr
	\code{runif.sobol} \tab Uniform scrambled Sobol sequence, \cr
	\code{rnorm.sobol} \tab Normal scrambled Sobol sequence, \cr
	\code{runif.pseudo} \tab Uniform pseudo random numbers, \cr
	\code{norma.pseudo} \tab Normal pseudo random numbers.}
            
}


\usage{
runif.halton(n, dimension, init) 
rnorm.halton(n, dimension, init)

runif.sobol(n, dimension, init, scrambling, seed) 
rnorm.sobol(n, dimension, init, scrambling, seed)

runif.pseudo(n, dimension, init)
rnorm.pseudo(n, dimension, init)
}


\arguments{

    \item{dimension}{
        an integer value, the dimension of the sequence. The 
        maximum value for the Sobol generator is 1111.
        }
    \item{init}{
        a logical, if TRUE the sequence is initialized and
        restarts, otherwise not. By default TRUE.
        }
    \item{n}{
        an integer value, the number of random deviates.
        }
    \item{scrambling}{
        an integer value, if 1, 2 or 3 the sequence is scrambled
        otherwise not. If 1, Owen type type of scrambling is 
        applied, if 2, Faure-Tezuka type of scrambling, is
        applied, and if 3, both Owen+Faure-Tezuka type of
        scrambling is applied. By default 0.
        }
    \item{seed}{
        an integer value, the random seed for initialization
        of the scrambling process. By default 4711. On effective
        if \code{scrambling>0}.
        }
        
}


\value{
  
    All generators return a numeric matrix of size \code{n} 
    by \code{dimension}.
    
}


\details{
    
    \bold{Halton's Low Discrepancy Sequences:}
    \cr\cr
    Calculates a matrix of uniform or normal deviated halton low 
    discrepancy numbers.
    \cr
    
    
    \bold{Scrambled Sobol's Low Discrepancy Sequences:}
    \cr\cr
    Calculates a matrix of uniform and normal deviated Sobol low 
    discrepancy numbers. Optional scrambling of the sequence can 
    be selected.
    \cr
    
    
    \bold{Pseudo Random Number Sequence:}
    \cr\cr
    Calculates a matrix of uniform or normal distributed pseudo 
    random numbers. This is a helpful function for comparing 
    investigations obtained from a low discrepancy series with 
    those from a pseudo random number.

}


\note{

    The global variables \code{runif.halton.seed} and 
    \code{runif.sobol.seed} save the status to restart the 
    generators. Note, that only one instance of a generators 
    can be run at the same time.
    
    The ACM Algorithm 659 implemented to generate scrambled 
    Sobol sequences is under the License of the ACM restricted 
    for academic and noncommerical usage. Please consult the 
    ACM License agreement included in the \code{doc} directory.
    
}


\author{
    
    P. Bratley and B.L. Fox for the Fortran Sobol Algorithm 659,\cr
    S. Joe for the Fortran extension to 1111 dimensions,\cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Bratley P., Fox B.L. (1988);
    \emph{Algorithm 659: Implementing Sobol's Quasirandom
    Sequence Generator},
    ACM Transactions on Mathematical Software 14, 88--100.

Joe S., Kuo F.Y. (1998);
    \emph{Remark on Algorithm 659: Implementing Sobol's Quaisrandom
    Seqence Generator}.
    
}


\examples{
## SOURCE("fOptions.D1-LowDiscrepancy")


## *.halton - 
   xmpOptions("\nStart: Uniform Holton Numbers > ")
   par(mfrow = c(2, 2), cex = 0.75)
   runif.halton(n = 10, dimension = 5)
   hist(runif.halton(n = 5000, dimension = 1), main = "Uniform Halton", 
     xlab = "x", col = "steelblue3", border = "white")
   xmpOptions("\nNext: Normal Holton Numbers > ")
   rnorm.halton(n = 10, dimension = 5)
   hist(rnorm.halton(n = 5000, dimension = 1), main = "Normal Halton", 
     xlab = "x", col = "steelblue3", border = "white")
   
## *.sobol - 
   xmpOptions("\nNext: Uniform Sobol Numbers > ")
   runif.sobol(n = 10, dimension = 5, scrambling = 3)
   hist(runif.sobol(5000, 1, scrambling = 2), main = "Uniform Sobol", 
     xlab = "x", col = "steelblue3", border = "white")
   xmpOptions("\nNext: Normal Sobol Numbers > ")
   rnorm.sobol(n = 10, dimension = 5, scrambling = 3)
   hist(rnorm.sobol(5000, 1, scrambling = 2), main = "Normal Sobol", 
     xlab = "x", col = "steelblue3", border = "white")
   
## *.pseudo - 
   xmpOptions("\nNext: Uniform Pseudo Random Numbers > ")
   runif.pseudo(n = 10, dimension = 5)
   rnorm.pseudo(n = 10, dimension = 5) 
}


\keyword{programming}

