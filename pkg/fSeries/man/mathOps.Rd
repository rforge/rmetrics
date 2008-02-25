\name{math}


\alias{math}


\alias{Ops.timeSeries}
\alias{Math.timeSeries}
\alias{Math2.timeSeries}
\alias{Summary.timeSeries}

\alias{diff.timeSeries}
\alias{scale.timeSeries}

\alias{quantile.timeSeries}


\title{Mathematical Time Series Operations}


\description{
  
  Functions and methods dealing with mathematical 'timeSeries'
  operations. \cr
  
  \tabular{ll}{  
    \code{Ops.timeSeries} \tab Group 'Ops' methods for a 'timeSeries'
    object \cr
    \code{Math.timeSeries} \tab Group 'Math' methods for a 'timeSeries'
    object \cr
    \code{Math2.timeSeries} \tab Group 'Math2' methods for a 'timeSeries'
    object \cr
    \code{Summary.timeSeries} \tab Group 'Summary' methods for a 'timeSeries'
    object \cr
    \code{diff} \tab Differences a 'timeSeries' object, \cr  
    \code{scale} \tab Centers and/or scales a 'timeSeries' object, \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'. }
  
}


\usage{
\method{Ops}{timeSeries}(e1, e2)
\method{Math}{timeSeries}(x, ...)
\method{Math2}{timeSeries}(x, digits)
\method{Summary}{timeSeries}(x, ..., na.rm = FALSE)
\method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)
\method{quantile}{timeSeries}(x, \dots)
}


\arguments{
  \item{center, scale}{ 
    [scale] - \cr
    either a logical value or a numeric vector of length equal to
    the number of columns of \code{x}.
  }
  \item{diff}{
    an integer indicating the order of the difference. By default 1.
  }
  \item{digits} {
    number of digits to be used in 'round' or 'signif'.
  }
  \item{e1, e2}{
    [Ops] - \cr   
    two objects of class \code{timeSeries}.
  } 
  \item{lag}{   
    an integer indicating which lag to use. By default 1.
  }
  \item{na.rm}{
    logical: should missing values be removed?
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

