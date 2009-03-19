\name{math}

\alias{math}

\alias{Ops,timeSeries,timeSeries-method}
\alias{Ops,array,timeSeries-method}
\alias{Ops,timeSeries,array-method}
\alias{Ops,timeSeries,vector-method}
\alias{Ops,vector,timeSeries-method}
\alias{-,timeSeries,missing-method}
\alias{+,timeSeries,missing-method}
\alias{cummax,timeSeries-method}
\alias{cummin,timeSeries-method}
\alias{cumprod,timeSeries-method}
\alias{cumsum,timeSeries-method}
\alias{Math,timeSeries-method}
\alias{Math2,timeSeries-method}
\alias{Summary,timeSeries-method}
\alias{trunc,timeSeries-method}
\alias{log,timeSeries-method}

\alias{\%*\%,timeSeries,vector-method}
\alias{\%*\%,timeSeries,ANY-method}
\alias{\%*\%,ANY,timeSeries-method}

\alias{diff,timeSeries-method}
\alias{scale,timeSeries-method}

\alias{quantile,timeSeries-method}

\alias{diff.timeSeries}
\alias{scale.timeSeries}

\alias{quantile.timeSeries}


\title{Mathematical Time Series Operations}


\description{
  
  Functions and methods dealing with mathematical 'timeSeries'
  operations. \cr
  
  \tabular{ll}{  
    \code{Ops-method} \tab Group 'Ops' methods for a 'timeSeries'
    object \cr
    \code{Math-method} \tab Group 'Math' methods for a 'timeSeries'
    object \cr
    \code{Math2-method} \tab Group 'Math2' methods for a 'timeSeries'
    object \cr
    \code{Summary-method} \tab Group 'Summary' methods for a 'timeSeries'
    object \cr
    \code{diff} \tab Differences a 'timeSeries' object, \cr  
    \code{scale} \tab Centers and/or scales a 'timeSeries' object, \cr
    \code{quantile} \tab Returns quantiles of an univariate 'timeSeries'. }
  
}


\usage{
% \S4method{Ops}{timeSeries}(e1, e2)
% \S4method{Math}{timeSeries}(x, ...)
% \S4method{Math2}{timeSeries}(x, digits)
% \S4method{Summary}{timeSeries}(x, ..., na.rm = FALSE)
\S4method{diff}{timeSeries}(x, lag = 1, diff = 1, trim = FALSE, pad = NA, \dots)
\S4method{scale}{timeSeries}(x, center = TRUE, scale = TRUE)
\S4method{quantile}{timeSeries}(x, \dots)
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
  
%   \item{digits} {
%     number of digits to be used in 'round' or 'signif'.
%   }
  
%   \item{e1, e2}{
%     [Ops] - \cr   
%     two objects of class \code{timeSeries}.
%   }
  
  \item{lag}{   
    an integer indicating which lag to use. By default 1.
  }

%   \item{na.rm}{
%     logical: should missing values be removed?
%   }

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
   setRmetricsOptions(myFinCenter = "GMT")
   charvec = timeCalendar()
   set.seed(4711)
   data = matrix(exp(cumsum(rnorm(12, sd = 0.1))))
   TS = timeSeries(data, charvec, units = "TS")
   TS

## Ops | +/- * ^ ...
   # Mathematical Operations:
   TS^2
   TS[2:4]
   OR = returns(TS)
   OR
   OR > 0
}


\keyword{chron}
\keyword{methods}
