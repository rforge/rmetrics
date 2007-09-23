\name{MathOperations}


\alias{MathOperations}


\alias{Ops.timeSeries}
\alias{abs.timeSeries}
\alias{sqrt.timeSeries}
\alias{exp.timeSeries}
\alias{log.timeSeries}
\alias{sign.timeSeries}
  

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
    \code{sign} \tab returns the signs of a 'timeSeries' object. }
    
}


\usage{
\method{Ops}{timeSeries}(e1, e2)
\method{abs}{timeSeries}(x)
\method{sqrt}{timeSeries}(x)
\method{exp}{timeSeries}(x)
\method{log}{timeSeries}(x, base = exp(1))
\method{sign}{timeSeries}(x)
}


\arguments{
  
    \item{base}{
        [log] - \cr
        a positive number. The base with respect to which logarithms
        are computed. Defaults to \code{e=exp(1)}.
        }
    \item{e1, e2}{
        [Ops] - \cr   
        two objects of class \code{timeSeries}.
        } 
    \item{x}{
        an object of class \code{timeSeries}.
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
   quantile(OR, probs = c(0.90, 0.95))
}


\keyword{chron}

