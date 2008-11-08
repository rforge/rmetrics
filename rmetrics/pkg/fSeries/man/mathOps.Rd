\name{math}


\alias{math}


\alias{Ops.timeSeries}
\alias{abs.timeSeries}
\alias{sqrt.timeSeries}
\alias{exp.timeSeries}
\alias{log.timeSeries}
\alias{sign.timeSeries}

\alias{diff.timeSeries}
\alias{scale.timeSeries}

\alias{quantile.timeSeries}
  

\title{Mathematical timeSeries Operations}


\description{
    
    A collection and description of functions 
    and methods dealing with mathematical timeSeries
    operations .        
    \cr
    
    \tabular{ll}{  
    \code{Ops.timeSeries} \tab S3: Arith method for a 'timeSeries' object, \cr
    \code{abs} \tab Returns absolute values of a 'timeSeries' object, \cr
    \code{sqrt} \tab Returns square root of a 'timeSeries' object, \cr
    \code{exp} \tab Returns the exponential values of a 'timeSeries' object, \cr
    \code{log} \tab Returns the logarithm of  a 'timeSeries' object, \cr
    \code{sign} \tab Returns the signs of a 'timeSeries' object, \cr
    \code{diff} \tab Differences a 'timeSeries' object, \cr 
    \code{scale} \tab Centers and/or scales a 'timeSeries' object, \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'. }
    
}


\usage{
\method{Ops}{timeSeries}(e1, e2)
\method{abs}{timeSeries}(x)
\method{sqrt}{timeSeries}(x)
\method{exp}{timeSeries}(x)
\method{log}{timeSeries}(x, base = exp(1))
\method{sign}{timeSeries}(x)
\method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)
\method{quantile}{timeSeries}(x, \dots)
}


\arguments{
  
    \item{base}{
        [log] - \cr
        a positive number. The base with respect to which logarithms
        are computed. Defaults to \code{e=exp(1)}.
        }
    \item{center, scale}{ 
        [scale] - \cr
        either a logical value or a numeric vector of length equal to
        the number of columns of \code{x}.
        }
    \item{diff}{
        an integer indicating the order of the difference. By default 1.
        }
    \item{e1, e2}{
        [Ops] - \cr   
        two objects of class \code{timeSeries}.
        } 
    \item{lag}{   
        an integer indicating which lag to use. By default 1.
        }     
    \item{pad}{
        [diffSeries] - \cr
        which value should get the padded values? By default \code{NA}.
        Another choice often used would be zero.
        } 
    \item{trim}{
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }
       
}


\value{

    returns the value from a mathematical or logical operation operating
    on objects of class \code{timeSeries}, or the value computed by a
    mathematical function.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 
   # Create an artificial timeSeries object:
   myFinCenter <<- "GMT"
   charvec = timeCalendar()
   set.seed(4711)
   data = matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   TS = timeSeries(data, charvec, units = "TS")
   TS

## Ops | +/- * ^ ...
   # Mathematical Operations:
   TS^2
   TS[2:4]
   OR = returnSeries(TS)
   OR
   OR > 0
}


\keyword{chron}

