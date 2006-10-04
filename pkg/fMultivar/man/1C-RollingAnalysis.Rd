\name{RollingAnalysis}

\alias{RollingAnalysis}

\alias{rollFun}
\alias{rollMin}
\alias{rollMax}
\alias{rollMean}
\alias{rollVar}

\title{Rolling Analysis}


\description{

    A collection and description of functions 
    to perform a rolling analysis. A rolling 
    analysis is often required in building trading 
    models.
    \cr
        
    The functions are:
    
    \tabular{ll}{
    \code{rollFun} \tab Rolling or moving sample statistics, \cr
    \code{rollMin} \tab Rolling or moving sample minimum, \cr
    \code{rollMax} \tab Rolling or moving sample maximum, \cr
    \code{rollMean} \tab Rolling or moving sample mean, \cr
    \code{rollVar} \tab Rolling or moving sample variance. }
            
}


\usage{
rollFun(x, n, trim = TRUE, na.rm = FALSE, FUN, ...)
rollMin(x, n = 9, trim = TRUE, na.rm = FALSE) 
rollMax(x, n = 9, trim = TRUE, na.rm = FALSE)
rollMean(x, n = 9, trim = TRUE, na.rm = FALSE)
rollVar(x, n = 9, trim = TRUE, unbiased = TRUE, na.rm = FALSE)
}


\arguments{

    \item{FUN}{
        the rolling function, arguments to this function can be
        passed through the \code{\dots} argument.
        }
    \item{n}{
        an integer specifying the number of periods or 
        terms to use in each rolling/moving sample.
        }
    \item{na.rm}{
        a logical flag: if TRUE, missing values in x will be removed 
        before computation. The default is FALSE.
        }
    \item{trim}{
        a logical flag: if TRUE, the first n-1 missing values in 
        the returned object will be removed; if FALSE, they will 
        be saved in the returned object. The default is TRUE.
        }
    \item{unbiased}{
        a logical flag. If TRUE, the unbiased sample variance 
        will be returned. The default is TRUE.
        }
    \item{x}{
        an univariate \code{timeSeries} object or a numeric vector.
        }
    \item{\dots}{
        additional arguments to be passed.
        }
    
}


\value{

    The functions return a \code{timeSeries} object or a numeric
    vector, depending on the argument \code{x}.
    
    \code{rollMax} returns the rolling sample maximum, \cr 
    \code{rollMin} returns the rolling sample minimum, \cr
    \code{rollMean} returns the rolling sample mean, and \cr
    \code{rollVar} returns the biased/unbiased rolling sample variance. 
    
    Note, that the function \code{rollFun} always returns a numeric
    vector, independent of the argument \code{x}.
    
    If you like to operate for \code{x} with rectangular objects, 
    you have to call the functions columnwise within a loop.

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\seealso{

    \code{\link{var}}.
    
}


\examples{
## SOURCE("fMultivar.1C-RollingAnalysis")

## Rolling Analysis:
\dontrun{
   xmpMultivar("\nNext: Rolling Analysis > ")
   x = (1:10)^2
   x
   trim =  c(TRUE, TRUE, FALSE, FALSE)
   na.rm = c(TRUE, FALSE, TRUE, FALSE)
   for (i in 1:4) 
     print(rollMin(x, 5, trim[i], na.rm[i]))
   for (i in 1:4) 
     print(rollMax(x, 5, trim[i], na.rm[i]))
   for (i in 1:4) 
     print(rollVar(x, 5, trim[i], unbiased = TRUE, na.rm[i]))
   for (i in 1:4) 
     print(rollVar(x, 5, trim[i], unbiased = FALSE, na.rm[i]))
}
}


\keyword{math}

