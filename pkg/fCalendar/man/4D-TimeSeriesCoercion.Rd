\name{TimeSeriesCoercion}

\alias{TimeSeriesCoercion}


\alias{is.timeSeries}

\alias{as.timeSeries}
\alias{as.timeSeries.default}
\alias{as.timeSeries.numeric}
\alias{as.timeSeries.data.frame} 
\alias{as.timeSeries.matrix}
\alias{as.timeSeries.ts}
\alias{as.timeSeries.character}
\alias{as.timeSeries.zoo} 

\alias{as.vector.timeSeries}
\alias{as.matrix.timeSeries}
\alias{as.data.frame.timeSeries}
\alias{as.ts.timeSeries}


\title{timeSeries Class, Coercion and Transformation}


\description{
    
    A collection and description of functions 
    and methods dealing with the coercion of
    'timeSeries' objects.       
    \cr
    
    Functions to create 'timeSeries' objects from other objects:
    
    \tabular{ll}{
    \code{as.timeSeries} \tab Generic function to convert a 'timeSeries' object, \cr
    \code{as.timeSeries.default} \tab Returns unchanged the 'timeSeries' object, \cr
    \code{as.timeSeries.numeric} \tab Converts from a numeric vector, \cr
    \code{as.timeSseries.data.frame} \tab Converts from a numeric vector, \cr
    \code{as.timeSeries.matrix} \tab Converts from a matrix, \cr
    \code{as.timeSeries.ts} \tab Converts from an object of class 'ts', \cr
    \code{as.timeSeries.character} \tab Converts from a named demo file, \cr
    \code{as.timeSeries.zoo} \tab Converts an object of class zoo. }

    Functions to transform 'timeSeries' objects into other objects:
    
    \tabular{ll}{
    \code{as.vector.timeSeries} \tab Coerces a 'timeSeries' to a vector, \cr
    \code{as.matrix.timeSeries} \tab Coerces a 'timeSeries' to a matrix, \cr
    \code{as.data.frame.timeSeries} \tab Coerces a 'timeSeries' to a data.frame, \cr
    \code{as.ts.timeSeries} \tab S3: Coerces a 'timeSeries' to a 'ts' object. }          
        
}


\usage{   
is.timeSeries(object)
\method{as.timeSeries}{numeric}(x, \dots)
\method{as.timeSeries}{data.frame}(x, \dots)
\method{as.timeSeries}{matrix}(x, \dots)
\method{as.timeSeries}{ts}(x, \dots)
\method{as.timeSeries}{character}(x, \dots)
\method{as.timeSeries}{zoo}(x, \dots)
 
\method{as.vector}{timeSeries}(x, mode = "any")
\method{as.matrix}{timeSeries}(x)
\method{as.data.frame}{timeSeries}(x, row.names = NULL, optional = NULL, \dots)
\method{as.ts}{timeSeries}(x, \dots)
}


\arguments{
  
    \item{mode}{ 
        a character string giving an atomic mode or \code{"list"}, 
        or (not for 'vector') \code{"any"}.
        }
    \item{object}{  
        an object of class \code{timeSeries}.
        }
    \item{optional}{
        A logical value. If \code{TRUE}, setting row names and converting 
        column names (to syntactic names) is optional.
        }
    \item{row.names}{
        \code{NULL} or a character vector giving the row names for the
        data frame. Missing values are not allowed.
        }
    \item{x}{ 
        an object which is coerced according to the generic function.
        } 
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{is.timeSeries}
    \cr
    returns \code{TRUE} or \code{FALSE} depending on whether its
    argument is of \code{timeSeries} type or not.
    \cr
    
    \code{as.timeSeries}\cr
    \cr
    returns a S4 object of class \code{timeSeries}.
    \cr
    
    \code{as.vector}\cr
    \code{as.data.frame}\cr
    \code{as.matrix}\cr
    \code{as.ts}
    \cr
    return depending on the generic function a numeric vector, a 
    data frame, a matrix, or an object of class \code{ts}.
        
}


\examples{
## SOURCE("fCalendar.4D-TimeSeriesCoercion")

## data - timeSeries:
   # Create an artificial timeSeries object:
   myFinCenter <<- "GMT"
   charvec = timeCalendar()
   data = matrix(rnorm(12))
   TS = timeSeries(data, charvec, units = "RAND")
   TS

## Test for timeSeries:
   is.timeSeries(TS)
   
## As Vector:
   as.vector(TS)
   
## As Matrix or Data Frame:
   as.matrix(TS)
   as.data.frame(TS)
   
## As Univariate Object of Class 'ts':
   as.ts(TS)
}
   

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

